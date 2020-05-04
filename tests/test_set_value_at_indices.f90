    program test_set_value_at_indices

    use bmif_2_0, only: BMI_SUCCESS, BMI_FAILURE
    use bmiprmsgroundwater
    use fixtures, only: config_file, status, print_1darray, isReal4EqualReal4, &
        isReal8EqualReal8, print_i_1darray, print_array, isintEqualint, print_d_1darray

    implicit none

    type (bmi_prms_groundwater) :: m
    integer :: retcode

    !test r32 gwsink_coef
    retcode = test1()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if

    !test r32 gwres_stor
    retcode = test2()
    if (retcode.ne.BMI_SUCCESS) then
        stop BMI_FAILURE
    end if

    contains

    ! Test getting r32 hru_area.
    function test1() result(code)
    character (len=*), parameter :: &
        var_name = "gwres_flow"
    integer, parameter :: rank =1
    integer, parameter :: size = 7
    integer, parameter :: fsize = 14
    integer, parameter, dimension(rank) :: shape = (/ 7 /)
    integer, parameter, dimension(rank) :: fshape = (/ 14 /)
    integer, parameter, dimension(size) :: &
        indices = (/ 2, 4, 6, 8, 10, 12, 14 /)

    real, parameter, dimension(fshape(1)) :: &
         expected = (/ 5.2482327E-03, 0.25, &
            4.2298837E-03, 0.25, 4.1147745E-03, 0.25, &
            3.0489832E-03, 0.25, 1.1592689E-03, 0.25, &
            2.0728302E-03, 0.25, 1.7155614E-03, 0.25 /)
    real :: val(size), fval(fsize)
    real :: setv(size), fsetv(fsize)
    integer :: i, code

    code = m%initialize(config_file)
    code = m%update()
    code = m%get_value(var_name, fval)
    code = m%get_value_at_indices(var_name, val, indices)
    val = 0.25
    code = m%set_value_at_indices(var_name, indices, val)
    code = m%get_value_at_indices(var_name, val, indices)
    code = m%get_value(var_name, fval)
    code = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 1"
    write(*,*) "Expected"
    call print_1darray(expected, fshape)
    write(*,*) "Set Value"
    call print_1darray(fval, fshape)

    code = BMI_SUCCESS
    do i = 1, fsize
        if (fval(i).ne.expected(i)) then
            code = BMI_FAILURE
        end if
    end do
    end function test1

    ! Test setting r64 gwres_stor.
    function test2() result(code)
    character (len=*), parameter :: &
        var_name = "gwres_stor"
    integer, parameter :: rank =1
    integer, parameter :: size = 7
    integer, parameter :: fsize = 14
    integer, parameter, dimension(rank) :: shape = (/ 7 /)
    integer, parameter, dimension(rank) :: fshape = (/ 14 /)
    integer, parameter, dimension(size) :: &
        indices = (/ 2, 4, 6, 8, 10, 12, 14 /)

    double precision, parameter, dimension(fshape(1)) :: &
         expected = (/ 0.127115771974441, 2.500000000000000e-003, &
                0.106442113395190, 2.500000000000000e-003, 0.115223223936042, 2.500000000000000e-003, &
                6.599501823096970e-002, 2.500000000000000e-003, 3.086473098702197e-002, 2.500000000000000e-003, &
                5.566617061204100e-002, 2.500000000000000e-003, 4.881643688461556e-002, 2.500000000000000e-003 /)
    double precision :: val(size), fval(fsize)
    double precision :: setv(size), fsetv(fsize)
    integer :: i, code

    code = m%initialize(config_file)
    code = m%update()
    code = m%get_value(var_name, fval)
    code = m%get_value_at_indices(var_name, val, indices)
    val = 0.0025d0
    code = m%set_value_at_indices(var_name, indices, val)
    code = m%get_value_at_indices(var_name, val, indices)
    code = m%get_value(var_name, fval)
    code = m%finalize()

    ! Visual inspection.
    write(*,*) "Test 2"
    write(*,*) "get Value"
    call print_d_1darray(fval, fshape)
    write(*,*) "Expected"
    call print_d_1darray(expected, fshape)

    code = BMI_SUCCESS
    do i = 1, fsize
        if (isReal8EqualReal8(fval(i), expected(i)).eqv..false.) then
            code = BMI_FAILURE
        end if
    end do
    end function test2

    end program test_set_value_at_indices
