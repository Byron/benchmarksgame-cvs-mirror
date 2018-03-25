// The Computer Language Benchmarks Game
// http://benchmarksgame.alioth.debian.org/
//
// Contributed by Sławomir Madej
// Added more threads

#define _GNU_SOURCE
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h>
#include <pthread.h>
#include <string.h>
#include <sys/stat.h>

char *pairs = "ATCGGCTAUAMKRYWWSSYRKMVBHDDHBVNN\n\n";
char tbl[128];

#define MAX_THREADS 16
#define WORKING_THREADS 8
#define WORKING_THREADS_IN (WORKING_THREADS - 1)

typedef struct {
   char *from;
   char *to;
   char *marker[16];
   int  markers;
   size_t off;
} thread_data_t;

void *set_offset(void *data)
{
   thread_data_t *td = data;
   char *from = td->from, *to = td->to;
   size_t off = td->off;

   char *m;
   for (m = from + 60 - off; m < to; m += 61) {
      memmove(m + 1, m, off);
      *m = '\n';
   }

   return NULL;
}

void *set_rc(void *data)
{
   thread_data_t *td = data;
   char *from = td->from, *to = td->to;

   size_t i;
   char c;
   for (to--, i = td->off; i; from++, to--, i--)
      c = tbl[(int)*from], *from = tbl[(int)*to], *to = c;

   return NULL;
}

void *process(void *data)
{
   thread_data_t *td = data;
   char *from = td->from, *to = td->to;

   from = memchr(from, '\n', to - from);
   from++;

   size_t len = to - from;
   size_t off = 60 - (len % 61);
   size_t lines = len / 61;
   size_t part = (len / ((WORKING_THREADS_IN + 1) * 2));

   pthread_t thread[WORKING_THREADS_IN];
   thread_data_t tdata[WORKING_THREADS_IN + 1];
   unsigned int t = WORKING_THREADS_IN;
   unsigned int i;

   if (off) {

      tdata[0].from = from;
      tdata[0].to = from + lines / (t + 1) * 61;
      tdata[0].off = off;

      for(i = 1; i < t; i++) {
         tdata[i].from = tdata[i-1].to;
         tdata[i].to = from + lines / (t + 1) * 61 * (i + 1);
         tdata[i].off = off;
      }
      tdata[t].from = tdata[t-1].to;
      tdata[t].to = to;
      tdata[t].off = off;

      i = t;
      while(i--)
         pthread_create(&thread[i], NULL, set_offset, &tdata[i]);
      
      set_offset(&tdata[t]);

      i = t;
      while(i--)
         pthread_join(thread[i], NULL);
   }

   tdata[0].from = from;
   tdata[0].to = to;
   tdata[0].off = part;

   for(i = 1; i < t; i++) {
      tdata[i].from = tdata[i-1].from + part;
      tdata[i].to = tdata[i-1].to - part;
      tdata[i].off = part;
   }

   from = tdata[t-1].from + part;
   to = tdata[t-1].to - part;

   i = t;
   while (i--)
      pthread_create(&thread[i], NULL, set_rc, &tdata[i]);

   char c;
   for (to--; from <= to; from++, to--)
      c = tbl[(int)*from], *from = tbl[(int)*to], *to = c;

   i = t;
   while (i--)
      pthread_join(thread[i], NULL);

   return 0;
}

void *get_markers(void *data)
{
   thread_data_t *td = data;
   char *from = td->from, *to = td->to;
   int marker = 0;

   while (1) {
      from = memrchr(from, '>', to - from);

      if (!from)
         break;

      td->marker[marker] = from;
      marker++;

      to = from;
      from = td->from;
   }

   td->markers = marker;

   return NULL;
}

int qs(const void *a, const void *b)
{
   a = *(char**)a;
   b = *(char**)b;

   if (a < b) return 1;
   if (a > b) return -1;

   return 0;
}

int main() {
   char *s;
   for (s = pairs; *s; s += 2) {
      tbl[toupper(s[0])] = s[1];
      tbl[tolower(s[0])] = s[1];
   }

   int in = fileno(stdin);

   struct stat st;
   fstat(in, &st);
   off_t size = st.st_size;
   char *buf = malloc(size + 1);
   buf[size] = '>';

   read(in, buf, size);

   char *from = buf, *to = buf + size - 1;

   pthread_t thread[MAX_THREADS];
   thread_data_t tdata[MAX_THREADS];
   unsigned int t = 0;
   int round = 0;
   int markersno = 0;
   char *markers[32];
   int i;

   t = WORKING_THREADS;

   size_t part = size / t;

   tdata[0].from = from;
   tdata[0].to = to - part * (t - 1);
   for (i = 1; i < t; i++) {
      tdata[i].from = tdata[i-1].to;
      tdata[i].to = to - part * (t - i - 1);
   }

   i = t;
   while(i--)
      pthread_create(&thread[i], NULL, get_markers, &tdata[i]);

   i = t;
   while(i--)
      pthread_join(thread[i], NULL);

   while(t--) {
      i = tdata[t].markers;
      while (i--) {
         markers[markersno + i] = tdata[t].marker[i];
      }
      markersno += tdata[t].markers;
   }

   markers[markersno] = to + 1;

   qsort(markers, markersno, sizeof(char*), qs);

   t = 0;
   to = buf + size - 1;

   while (1) {
      from = markers[t];
      to = markers[(t ? t - 1 : markersno)] - 1;

      if (t == MAX_THREADS) {
         round = 1;
         t = 0;
      }

      if (round)
         pthread_join(thread[t], 0);   

      tdata[t].from = from;
      tdata[t].to = to;

      pthread_create(&thread[t], NULL, process, &tdata[t]);
      t++;

      to = from - 1;
      if (to < buf) break;
   }

   if (round)
      t = MAX_THREADS;

   while (t--)
      pthread_join(thread[t], NULL);

   write(fileno(stdout), buf, size);
   free(buf);

   return 0;
}
