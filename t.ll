; ModuleID = 'Micro Scheme'


 


@x =    global i32 0


@y =    global i32 0

@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1


define external ccc  i32 @foo(i32  %a_0, i32  %b_0, i32  %c_0, i32  %d_0)    {
entry_0:
  %0 = alloca i32 
  store  i32 %a_0, i32* %0 
  %1 = alloca i32 
  store  i32 %b_0, i32* %1 
  %2 = alloca i32 
  store  i32 %c_0, i32* %2 
  %3 = alloca i32 
  store  i32 %d_0, i32* %3 
  %4 = load  i32, i32* %0 
  %5 = load  i32, i32* %1 
  %6 = load  i32, i32* %2 
  %7 = load  i32, i32* %3 
  %8 = mul   i32 %4, %5 
  %9 = mul   i32 %8, %6 
  %10 = mul   i32 %9, %7 
  ret i32 %10 
}

define i32 @main() #0 {
  %1 = call i32 @foo(i32 1, i32 2, i32 3, i32 4)
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i64 0, i64 0), i32 %1)
  ret i32 0
}
declare i32 @printf(i8*, ...) #1
