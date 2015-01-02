#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

struct string {
  int32_t len;
  char *buf;
};

typedef struct string string;

string *string_new(char *buf, int32_t len) {
  string *s = malloc(sizeof(string));
  s->len = len;
  s->buf = buf;
  return s;
}

void string_puts(string *s) {
  puts(s->buf);
}

int string_len(string *s) {
  return s->len;
}

float string_to_float(string *s) {
  return atof(s->buf);
}

int string_to_int(string *s) {
  return atoi(s->buf);
}

string *string_add(string *s, string *t) {
  int len = s->len + t->len;
  char *buf = malloc(len + 1);
  memcpy(buf, s->buf, s->len);
  memcpy(buf + s->len, t->buf, t->len);
  buf[len] = '\0';
  return string_new(buf, len);
}
