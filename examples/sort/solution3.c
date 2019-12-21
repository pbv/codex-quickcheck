/* 
 * Sort descreasing, using bubblesort; incorrect solution
 */
void sort_desc(int vec[], int n)
{
  int k;
  do {
    k = 0;
    for(int j = 1; j <= n; j++) {  
      if(vec[j-1] < vec[j]) {
	int temp = vec[j];
	vec[j] = vec[j-1];	
	vec[j-1] = temp;
	k = j;
      }
    }
    n = k;
  } while(n > 0);
}
