/* -*- sm-c -*- */

#define toto /* bla
                bla */ \
 if (a) { \
   f \
 }

DEFUN ()

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
      printf ("weee\n");
    else
      printf ("wop\n");
    
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

static struct myownspecialstruct
  *testfunction
  (args)
{
  return NULL;
}
