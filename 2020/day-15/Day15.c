#include <stdio.h>
#include <stdlib.h>

int
main(void) {
  int *a = malloc(31000000 * sizeof(*a));
  for (int i = 0 ; i < 31000000 ; i++)
    a[i] = -1;
  int input[] = {1,0,18,10,19,6};
  for (int i = 0 ; i < 5 ; i++)
    a[input[i]] = i+1;
  int prev = 6;
  for (int t = 6 ; ; t++) {
    int n;
    if (a[prev] == -1) {
      n = 0;
    } else {
      n = t - a[prev];
    }
    a[prev] = t;
    if (t == 2020) {
      printf("%d\n", prev);
    } else if (t == 30000000) {
      printf("%d\n", prev);
      return 0;
    }
    prev = n;
  }
}
