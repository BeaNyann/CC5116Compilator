#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>
#include <string.h>
#include <stdbool.h>
#include <inttypes.h>
#include <sys/resource.h>

/* synonym to ease typing/reading */
typedef uint64_t u64;

/* configuration */
u64 STACK_SIZE = 0x800000;
u64 HEAP_SIZE = 16;
int USE_GC = 1;

/* externs */
extern u64* try_gc(u64* alloc_ptr, u64 words_needed, u64* cur_frame, u64* cur_sp) asm("try_gc");
extern u64 our_code_starts_here(u64* heap) asm("our_code_starts_here");
extern void set_stack_bottom(u64* stack_bottom) asm("set_stack_bottom");
/* */

const u64 BOOL_TRUE =     0xFFFFFFFFFFFFFFFF;
const u64 BOOL_FALSE =    0x7FFFFFFFFFFFFFFF;
const u64 BOOL_TAG =      0x0000000000000001;
const u64 CLOSURE_TAG =   0x0000000000000005;

const int ERR_NOT_NUMBER = 1;
const int ERR_NOT_BOOLEAN = 2;
const int ERR_NOT_TUPLE = 3;
const int ERR_INDEX_TOO_LOW = 4;
const int ERR_INDEX_TOO_HIGH = 5;
const int ERR_WRONG_ARITY = 6;
const int ERR_NOT_CLOSURE = 7;

// other error codes here
bool is_tuple(u64 val){
  return ((val & BOOL_TAG) == BOOL_TAG);
}
bool is_closure(u64 val){
  return ((val & CLOSURE_TAG) == CLOSURE_TAG);
}
void error(u64 val, u64 errCode) {
  if (errCode == ERR_NOT_NUMBER) {
    if (val == BOOL_TRUE) {
        printf( "Expected number, but got true");
    } else { 
        printf("Expected number, but got false");
    }
  } else if (errCode == ERR_NOT_BOOLEAN) {
    printf("Expected boolean, but got %ld", ((int64_t)(val)) / 2);
  } else if (errCode == ERR_NOT_TUPLE) {
    if (val == BOOL_TRUE) {
        printf( "Expected pair, but got true");
    } else if (val == BOOL_FALSE){ 
        printf("Expected pair, but got false");
    } else {
      printf("Expected pair, but got %ld", ((int64_t)(val)) / 2);
    }
  } else if (errCode == ERR_INDEX_TOO_LOW){
    printf("Index out of bounds, too low");
  } else if (errCode == ERR_INDEX_TOO_HIGH){
    printf("Index out of bounds, too high");
  } else if (errCode == ERR_WRONG_ARITY){
    printf("Wrong arity");
  } else if (errCode == ERR_NOT_CLOSURE){
    printf("Not closure %ld", ((int64_t)(val)));
  }
  exit(errCode);
}

void print_help(u64 val, char* prelude, char* fugue){
  if ((val & BOOL_TAG) == 0) { // val is even ==> number
      printf("%s%ld%s", prelude, ((int64_t)(val)) / 2, fugue);
  } else if (val == BOOL_TRUE) {
      printf("%strue%s", prelude, fugue);
  } else if (val == BOOL_FALSE) { 
      printf("%sfalse%s", prelude, fugue);
  } else if ((val & CLOSURE_TAG) == CLOSURE_TAG){
    printf("<fun>");
  } else if ((val & BOOL_TAG) == BOOL_TAG) {
      val = val - BOOL_TAG;
      u64 arity = *(u64*)val;
      printf("(");
      for (u64 i = 1; i <= arity; i++) {
          val = val + 8;
          print_help( (*(u64*)val) , "", "");
          if (i!= arity) printf(",");
      }
      printf(")");
  } else {
      printf("%sUnknown value: %#018x%s", prelude, val, fugue); // print unknown val in hex
  }
}



u64 print(u64 val) {
  print_help(val, "> ","\n");
  return val;
}



u64 identity(u64 val) {
  return val;
}

int max_int(int val1, int val2) {
  if (val1 >= val2){
    return val1;
  } else {
    return val2;
  }
}
 int if_int(int cond, int val1, int val2){
   if (cond) {
     return val1;
   } else
   {
     return val2;
   }
 }

 int multiargs(int a1, int a2, int a3, int a4, int a5, int a6, int a7){
   return a3 + a4;
 }

 int multiargs2(int a1, int a2, int a3, int a4, int a5, int a6){
   return a3 + a4;
 }

/* GC */
u64* HEAP_START;
u64* HEAP_END;
u64* HEAP_MID;
u64* TO_SPACE;
u64* FROM_SPACE;
u64* ALLOC_PTR = 0;
u64* SCAN_PTR = 0;
u64* STACK_BOTTOM = 0;
int iter = 0;

void set_stack_bottom(u64* stack_bottom) {
  STACK_BOTTOM = stack_bottom;
}

bool is_heap_ptr(u64 val){
  return (u64*)val < HEAP_END && (u64*)val >= HEAP_START;
}

bool is_old_heap_ptr(u64* FROM_SPACE, u64 val){
  if(FROM_SPACE == HEAP_MID){
    return (u64*)val < HEAP_END && (u64*)val >= HEAP_MID;
  }
  else if(FROM_SPACE == HEAP_START){
    return (u64*)val < HEAP_MID-1 && (u64*)val >= HEAP_START;
  }
}

void print_stack(u64* rbp, u64* rsp) {
  printf("|------- frame %p to %p  ------\n", rsp, rbp);
  for (u64* cur_word = rsp; cur_word < rbp; cur_word++) {
    u64 val = (u64)*cur_word;
    printf("| %p: %p", cur_word, (u64*)*cur_word);
    if (is_heap_ptr(val)) {
      if (is_closure(val)){ printf(" (closure)"); }
      else if (is_tuple(val)){ printf(" (tuple)"); }
    }
    printf("\n");
  }
  if (rbp < STACK_BOTTOM) {
    print_stack((u64*)*rbp, rbp + 2);
  }
  else {
    printf("|------- bottom %p  ------\n\n", STACK_BOTTOM);
  }
}

void print_heap(u64* heap_start, u64* heap_end){
  printf("| Heap from %p to %p\n", heap_start, heap_end);
  for (u64 i = 0; i <= (u64)(heap_end - heap_start); i++) {
    printf("|  %lld/%p: %p \n", i, (heap_start + i), (u64*)*(heap_start + i));
  }
}

void print_heaps(){
  printf("|\n|=======HEAP 1==========\n");
  print_heap(HEAP_START, HEAP_MID - 1);
  printf("|=======HEAP 2==========\n");
  print_heap(HEAP_MID, HEAP_END);
  printf("|=================\n\n");
}

void copy(int tag, u64 dir, u64* cur_word, bool isTuple){
  for (u64 j = 0; j <= HEAP_SIZE; j++) {
    if ((TO_SPACE + j) == dir - (unsigned long)tag){
      *cur_word = (uint64_t*) ((*(unsigned long*)&ALLOC_PTR+tag));
      u64 val = isTuple ? (u64)*(TO_SPACE + j) : (u64)*(TO_SPACE + j + 2) + (u64)2;
      if(val%(u64)2==0) val++;
      for(u64 m = 0; m<=val; m++){
        *(ALLOC_PTR) = *(TO_SPACE + j + m);
        ALLOC_PTR++;
      }
    }
  }
}

void scan_stack(u64* cur_sp, u64* cur_frame){
  for (u64* cur_word = cur_sp; cur_word < cur_frame; cur_word++) {
    u64 dir = (u64)*cur_word;
    if (is_heap_ptr(dir)) {
      if (is_closure(dir)){
        copy(5,dir,cur_word, false);
    } else if (is_tuple(dir)){
        copy(1,dir,cur_word, true);
      }
    }
  }
  if (cur_frame < STACK_BOTTOM){
    scan_stack(cur_frame + 2, (u64*)*cur_frame);
  }
}
u64* collect(u64* cur_frame, u64* cur_sp) {
  // swap from-space to-space
  u64* temp = TO_SPACE;
  TO_SPACE = FROM_SPACE;
  FROM_SPACE = temp;
  ALLOC_PTR = FROM_SPACE;
  // scan stack and copy roots
  scan_stack(cur_sp, cur_frame);
  //scan heap
  for (u64 k = 0; k <= HEAP_SIZE; k++) {
    u64 dir = (u64)* (FROM_SPACE + k);
    if(is_old_heap_ptr(TO_SPACE, dir)){
      if (is_closure(dir)){
        copy(5,dir,FROM_SPACE + k, false);
    } else if (is_tuple(dir)){
        copy(1,dir,FROM_SPACE + k, true);
      }
    }
  } 
  
  // clean old space
  for (u64 j = 0; j <= (u64)HEAP_SIZE; j++) {
    *(TO_SPACE + j) = NULL;
  }
  return ALLOC_PTR;
}

/* trigger GC if enabled and needed, out-of-memory error if insufficient */
u64* try_gc(u64* alloc_ptr, u64 words_needed, u64* cur_frame, u64* cur_sp) {
  if (USE_GC==1 && alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE) {
    printf("| need memory: GC!\n");
    alloc_ptr = collect(cur_frame, cur_sp);
  }
  if (alloc_ptr + words_needed > FROM_SPACE + HEAP_SIZE) {
    printf("| Error: out of memory!\n\n");
    print_stack(cur_frame, cur_sp);
    print_heaps();
    exit(-1);
  }
  return alloc_ptr;
}

/* start */
int main(int argc, char** argv){

  /* stack size config */
  char* stack_size_envvar = getenv("STACK_SIZE");
  if (stack_size_envvar) STACK_SIZE = strtoull(stack_size_envvar, NULL, 0);
  printf("| Setting stack size to %" PRId64 " .\n", STACK_SIZE);
  struct rlimit limit;
  getrlimit(RLIMIT_STACK, &limit);
  limit.rlim_cur = STACK_SIZE < limit.rlim_max ? STACK_SIZE : limit.rlim_max;
  int res = setrlimit(RLIMIT_STACK, &limit);
  if (res != 0) { printf("| Setting rlimit failed...\n") ; }

  /* GC config */
  char* use_gc_envvar = getenv("USE_GC");
  if (use_gc_envvar) USE_GC = strtoull(use_gc_envvar, NULL, 0);
  printf("| Use GC: %d\n", USE_GC);

  /* heap size config */
  char* heap_size_envvar = getenv("HEAP_SIZE");
  if (heap_size_envvar) HEAP_SIZE = strtoull(heap_size_envvar, NULL, 0);
  printf("| Heap size: %" PRId64 " .\n", HEAP_SIZE);

  /* setting up two space heap for GC */
  u64* heap = (u64*)calloc((HEAP_SIZE * 2) + 15, sizeof(u64));
  HEAP_START = (u64*)(((u64)heap + 15) & ~0xF);
  /* TBD: initialize HEAP_MID, HEAP_END, FROM_SPACE, TO_SPACE */
  HEAP_MID = (u64*)(((u64) HEAP_START + 8 * HEAP_SIZE));   /* TBD */
  HEAP_END = (u64*)(((u64) HEAP_START + 16 * HEAP_SIZE - 1));   /* TBD */
  FROM_SPACE = HEAP_START; /* TBD */
  TO_SPACE = HEAP_MID;   /* TBD */

  /* Go! */
  /* Q: when do you need to call `free(heap)`? */
  // printf("entre al main");
  u64 result = our_code_starts_here(heap);
  // printf("Sali del main");
  typedef uint64_t VAL;
  print_help(result, "","\n");
  free(heap);
  return 0;
}
