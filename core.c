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

int string_printf(array *s, ...) {
  va_list args;
  va_start(args,s);
  int ret = vprintf(s->buf,args);
  va_end(args);
  fflush(stdout);
  return ret;
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

int string_compare(array *s, array *t) {
  return strcmp(s->buf, t->buf) == 0;
}

int string_to_int(array *s) {
  return atoi(s->buf);
}

array *array_add(array *s, array *t) {
  int len = s->len + t->len;
  char *buf = malloc(s->size * len + 1);
  memcpy(buf, s->buf, s->size * s->len);
  memcpy(buf + s->size * s->len, t->buf, t->size * t->len);
  buf[s->size * len] = '\0';
  return array_new(buf, len, s->size);
}
