! Fortran 90 Allocatable and Pointer Arrays
! Demonstrates ALLOCATABLE and POINTER attributes
! Filename: dynamic_arrays.f90

module memory_management
    implicit none
    private
    public :: allocate_matrix, deallocate_matrix, copy_matrix
    
contains
    
    ! Allocate a new matrix
    subroutine allocate_matrix(matrix, rows, cols)
        real, allocatable, intent(out) :: matrix(:,:)
        integer, intent(in) :: rows, cols
        
        if (allocated(matrix)) then
            deallocate(matrix)
        end if
        
        allocate(matrix(rows, cols))
        matrix = 0.0
    end subroutine allocate_matrix
    
    ! Deallocate matrix
    subroutine deallocate_matrix(matrix)
        real, allocatable, intent(inout) :: matrix(:,:)
        
        if (allocated(matrix)) then
            deallocate(matrix)
        end if
    end subroutine deallocate_matrix
    
    ! Copy one matrix to another
    subroutine copy_matrix(source, destination)
        real, intent(in) :: source(:,:)
        real, allocatable, intent(out) :: destination(:,:)
        integer :: rows, cols
        
        rows = size(source, 1)
        cols = size(source, 2)
        
        allocate(destination(rows, cols))
        destination = source
    end subroutine copy_matrix
    
end module memory_management

! Main program with pointer examples
program dynamic_array_demo
    use memory_management
    implicit none
    
    real, allocatable :: A(:,:), B(:,:)
    real, pointer :: ptr(:) => null()
    real, target :: array(10)
    integer :: i, j, status
    
    ! Allocate matrix A
    print *, "Allocating matrix A (5x5)..."
    call allocate_matrix(A, 5, 5)
    
    ! Initialize A
    do i = 1, 5
        do j = 1, 5
            A(i, j) = real(i + j)
        end do
    end do
    
    print *, "Matrix A initialized"
    
    ! Copy to B
    print *, "Copying A to B..."
    call copy_matrix(A, B)
    
    print *, "B(1,1) =", B(1,1)
    
    ! Pointer example
    print *, "Setting up pointer..."
    do i = 1, 10
        array(i) = real(i * i)
    end do
    
    ptr => array
    
    print *, "Pointer points to array(1) =", ptr(1)
    print *, "Pointer sum =", sum(ptr)
    
    ! Clean up
    call deallocate_matrix(A)
    call deallocate_matrix(B)
    
    print *, "Deallocated matrices"
    print *, "Array still accessible:", array(5)
    
    ! Nullify pointer
    nullify(ptr)
    print *, "Pointer nullified"
    
end program dynamic_array_demo
