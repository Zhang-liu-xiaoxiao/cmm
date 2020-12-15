/*
    cmm language interpreter
    include follows features
    multiline notes
    single line notes
    nested while loop
    if judge
    composed exp
    assign variable
    nested
    variable scope ,inner scope and outer scope support
    inner overwrite outer
    can detect repeated variable
*/


void main()
{
  int sum=0;
  int d=5;
  //int d=3; // error repeated variable
  while((sum+1)<3)   //while statement and complex exp
  {
    int d = sum+1;   //inner variable overwrite outer d
    write d;        //inner d
    sum = sum+1;
    while(d<5){   //nested while
        d=d+1;
    }
  }
  write d;      //outer d
  write sum;
  int c;
  if(sum>d){   //if statement
      c=0;
  }
  else {
      c=1;
  }
  write c;
}

