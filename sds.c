#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

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
