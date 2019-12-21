/*
 * Sort decreasing, incorrect solution 1
 */

void sort_desc(int vec[], int n)
{
  int i, j;
  for(i = 0; i < n; i++) {
    int imin = i;
    for(j = i+1; j < n; j++) {  
      if(vec[j] > vec[imin]) imin = j;
    }
    if(imin != i) {
      int temp = vec[i];
      vec[imin] = temp;
      vec[i] = vec[imin];
    }
  }
}
