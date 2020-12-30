define i1 @foo(i1 %0, i1 %1) {
  %3 = alloca i8
  %4 = alloca i8
  %5 = zext i1 %0 to i8
  store i8 %5, i8* %3
  %6 = zext i1 %1 to i8
  store i8 %6, i8* %4
  %7 = load i8, i8* %4
  %8 = trunc i8 %7 to i1
  br i1 %8, label %12, label %9

9:
  %10 = load i8, i8* %3
  %11 = trunc i8 %10 to i1
  br label %12

12:
  %13 = phi i1 [ true, %2 ], [ %11, %9 ]
  ret i1 %13
}

define i32 @main() #0 {
  call i1 @foo(i1 true, i1 false)
  ret i32 0
}
