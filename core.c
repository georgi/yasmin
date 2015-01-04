#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>

struct array {
  int32_t len;
  size_t size;
  char *buf;
};

typedef struct array array;

array *array_new(char *buf, int32_t len, size_t size) {
  array *s = malloc(sizeof(array));
  s->len = len;
  s->size = size;
  s->buf = buf;
  return s;
}

void string_puts(array *s) {
  puts(s->buf);
}

int array_len(array *s) {
  return s->len;
}

float string_to_float(array *s) {
  return atof(s->buf);
}

int string_to_int(array *s) {
  return atoi(s->buf);
}

array *array_add(array *s, array *t) {
  int len = s->len + t->len;
  char *buf = malloc(s->size * len + 1);
  memcpy(buf, s->buf, s->size * s->len);
  memcpy(buf + s->len, t->buf, t->size * t->len);
  buf[len] = '\0';
  return array_new(buf, len, s->size);
}
