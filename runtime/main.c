#include <stdio.h>
#include <stdlib.h>

void ___read_int8(int8_t *a) {
  scanf("%hhd", a);
}

void ___read_int16(int16_t *a) {
  scanf("%hd", a);
}

void ___read_int32(int32_t *a) {
  scanf("%d", a);
}

void ___read_int64(int64_t *a) {
  scanf("%lld", a);
}

void ___read_float32(float *a) {
  scanf("%f", a);
}

void ___read_float64(double *a) {
  scanf("%lf", a);
}

void ___print_bool(int8_t a) {
  puts(a ? "true" : "false");
}

void ___print_int8(int8_t a) {
  printf("%d\n", a);
}

void ___print_int16(int16_t a) {
  printf("%d\n", a);
}

void ___print_int32(int32_t a) {
  printf("%d\n", a);
}

void ___print_int64(int64_t a) {
  printf("%lld\n", a);
}

void ___print_float32(float a) {
  printf("%f\n", a);
}

void ___print_float64(double a) {
  printf("%f\n", a);
}
