! Fortran 90 Array Allocation Example
! Demonstrates ALLOCATE, DEALLOCATE, and array operations
! Filename: array_operations.f90

module array_utils
    implicit none
    private
    public :: matrix_sum, matrix_multiply, print_matrix
    
contains
    
    ! Sum all elements in a matrix
    real function matrix_sum(matrix)
        real, dimension(:,:), intent(in) :: matrix
        integer :: i, j, rows, cols
        
        rows = size(matrix, 1)
        cols = size(matrix, 2)
        
        matrix_sum = 0.0
        do i = 1, rows
            do j = 1, cols
                matrix_sum = matrix_sum + matrix(i, j)
            end do
        end do
    end function matrix_sum
    
    ! Print matrix to console
    subroutine print_matrix(label, matrix)
        character(len=*), intent(in) :: label
        real, dimension(:,:), intent(in) :: matrix
        integer :: i, j
        
        print *, label
        do i = 1, size(matrix, 1)
            print *, matrix(i, :)
        end do
    end subroutine print_matrix
    
    ! Multiply two matrices
    function matrix_multiply(A, B) result(C)
        real, dimension(:,:), intent(in) :: A, B
        real, dimension(:,:), allocatable :: C
        integer :: m, n, k, i, j, p
        
        m = size(A, 1)  ! rows of A
        n = size(A, 2)  ! cols of A = rows of B
        k = size(B, 2)  ! cols of B
        
        allocate(C(m, k))
        
        do i = 1, m
            do j = 1, k
                C(i, j) = 0.0
                do p = 1, n
                    C(i, j) = C(i, j) + A(i, p) * B(p, j)
                end do
            end do
        end do
    end function matrix_multiply
    
end module array_utils

! Main program demonstrating array allocation
program array_demo
    use array_utils
    implicit none
    
    real, allocatable :: data(:,:)
    real, allocatable :: result(:,:)
    integer :: n, m, i, j
    
    ! Allocate matrices
    n = 3
    m = 3
    
    allocate(data(n, m))
    
    ! Initialize matrix
    do i = 1, n
        do j = 1, m
            data(i, j) = real(i * j)
        end do
    end do
    
    ! Print original matrix
    call print_matrix("Original Matrix:", data)
    
    ! Calculate sum
    print *, "Sum of all elements:", matrix_sum(data)
    
    ! Matrix multiplication (square matrix times itself)
    result = matrix_multiply(data, data)
    call print_matrix("Result of A*A:", result)
    
    ! Deallocate
    deallocate(data)
    deallocate(result)
    
    print *, "Arrays deallocated successfully"
    
end program array_demo
