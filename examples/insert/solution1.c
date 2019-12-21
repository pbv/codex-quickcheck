
void insert_value(int vec[], int n, int val)
{
  int i = n-1;
  while(i>=0 && vec[i] > val) {
    vec[i+1] = vec[i];
    i--;
  }
  vec[i+1] = val;
}
