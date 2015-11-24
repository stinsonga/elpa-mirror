/* -*- sm-c -*- */

#define toto(arg) /* bla
                     bla */ \
  if (a) {  /* toto
             * titi
             */    \
    fs((arg) + 2); \
  }

#define test(arg) \
  (hello + arg)

struct foo;

#define titi(arg) { \
    if (a) {        \
      f(arg + 1)    \
    }               \
  }

DEFUN ()
  ()
{
  return Qnil;
}

int main (void)
{
  if (a)
    do
      if (b)
        if (c)
          printf ("hello\n");
        else
          printf ("there\n");
      else
        printf ("elsewhere\n");        
    while (6);
  else if (b)
    printf ("wow\n");
  else
    if (c)
      printf
        ("weee\n");
    else
      printf ("wop\n");

  if (a)
    if (b) {
      c;
    }

  *a = b;

  if (pITORIG != pITCOPY)
    *(pITORIG)
      = *(pITCOPY);

  switch (a)
    {
    case 1:
      {
        if (a)
          {
            y = 5;
    case 2:
            x = 3;
          }
      }
    }
}

static struct myownspecialstruct *
testfunction
  (args)
{
  return NULL;
}
