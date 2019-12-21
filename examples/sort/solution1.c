/*
 * Sort decreasing using Selection sort
 */
void sort_desc(int vec[], int n)
{
  int i, j;
  for(i = 0; i < n; i++) {
    int imin = i; // índice inicial do mínimo
    for(j = i+1; j < n; j++) {
      if(vec[j] > vec[imin]) imin = j;
    }
    // trocar o mínimo com vec[i]
    if(imin != i) {
      int temp = vec[i];
      vec[i] = vec[imin];
      vec[imin] = temp;
    }
  }
}
