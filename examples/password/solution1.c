/*
 * Example solution 1, incorrect
 *  (assumes all characters are letters or digits)
 */

#include <string.h>
#include <ctype.h>

int strong_passwd(char str[])
{
   int n, count1=0, count2=0, count3=0;
   n = strlen(str);
   if(n<6) {
     return 0;
   }
   for(int i=0;i<n;i++){
     if(isupper(str[i])){
       count1++;
     } else if(islower(str[i])){
       count2++; 
     } else if(!isalpha(str[i])){
       count3++;
     }
  }
  if(count1>=1 && count2>=1 && count3>=1){
      return 1;
  }
  return 0;
}
