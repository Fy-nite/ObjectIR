program test_subroutine
    implicit none
    call greet()
end program test_subroutine

subroutine greet()
    print *, "Hello from subroutine!"
end subroutine greet
