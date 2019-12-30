! Test the lifecycle and time BMI methods.
program irf_test

  use bmif_2_0, only: BMI_MAX_UNITS_NAME, BMI_SUCCESS, BMI_FAILURE
  use bmiprmssurface
  use bmiprmssoil
  use bmiprmsgroundwater
  implicit none

  type (bmi_prms_surface) :: m_surf
  type (bmi_prms_soil) :: m_soil
  type (bmi_prms_groundwater) :: m_gw
  integer :: s, i
  double precision :: time, time0, time1
  character (len=BMI_MAX_UNITS_NAME) :: time_units
    character (len=*), parameter :: control_file = './pipestem/control.simple1'
  character (len=*), parameter :: control_file1 = './pipestem_surface/control.simple1'
  character (len=*), parameter :: control_file2 = './pipestem_soil/control.simple1'
  character (len=*), parameter :: control_file3 = './pipestem_groundwater/control.simple1'
  double precision :: endtime

  write (*,"(a)",advance="no") "Initializing..."
  s = m_surf%initialize(control_file1)
  s = m_soil%initialize(control_file2)
  s = m_gw%initialize(control_file3)
  !s = surface2soil(m_surf, m_soil)
  s = m_surf%get_end_time(endtime)
  do i = 1,int(endtime)
      s = m_surf%update()
      s = surface2soil(m_surf, m_soil)
      s = m_soil%update()
      s = soil2surface(m_surf, m_soil)
  enddo
  write (*,*) "Done."


  write (*,"(a)", advance="no") "Finalizing..."
  s = m_surf%finalize()
  s = m_soil%finalize()
  write (*,*) "Done"
  
    contains
    function soil2surface(msurf, msoil) result(code)
        type (bmi_prms_surface), intent(inout) :: msurf
        type (bmi_prms_soil), intent(in) :: msoil
        real, allocatable, dimension(:) :: r32var
        integer, allocatable, dimension(:) :: i32var
        double precision, allocatable, dimension(:) :: r64var
        integer :: code
        integer :: gridid1,gridid2, nelem, nelem1, nelem2
        
        !double precision
        nelem  = getvarsize(msurf, msoil, 'basin_sroff')
        call allocr64var(r64var, nelem)
        code = msoil%get_value('basin_sroff', r64var)
        code = msurf%set_value('basin_sroff', r64var)
        
        !nelem  = getvarsize(msurf, msoil, 'dprst_seep_hru')
        !call allocr64var(r64var, nelem)
        !code = msoil%get_value('dprst_seep_hru', r64var)
        !code = msurf%set_value('dprst_seep_hru', r64var)
        
        !nelem  = getvarsize(msurf, msoil, 'strm_seg_in')
        !call allocr64var(r64var, nelem)
        !code = msoil%get_value('strm_seg_in', r64var)
        !code = msurf%set_value('strm_seg_in', r64var)
        !
        nelem  = getvarsize(msurf, msoil, 'basin_potet')
        call allocr64var(r64var, nelem)
        code = msoil%get_value('basin_potet', r64var)
        code = msurf%set_value('basin_potet', r64var)

        !reals

        !nelem  = getvarsize(msurf, msoil, 'dprst_evap_hru')
        !call allocr32var(r32var, nelem)
        !code = msoil%get_value('dprst_evap_hru', r32var)
        !code = msurf%set_value('dprst_evap_hru', r32var)

        !nelem  = getvarsize(msurf, msoil, 'hru_area_perv')
        !call allocr32var(r32var, nelem)
        !code = msoil%get_value('hru_area_perv', r32var)
        !code = msurf%set_value('hru_area_perv', r32var)

        !nelem  = getvarsize(msurf, msoil, 'hru_impervevap')
        !call allocr32var(r32var, nelem)
        !code = msoil%get_value('hru_impervevap', r32var)
        !code = msurf%set_value('hru_impervevap', r32var)

        nelem  = getvarsize(msurf, msoil, 'infil')
        call allocr32var(r32var, nelem)
        code = msoil%get_value('infil', r32var)
        code = msurf%set_value('infil', r32var)

        !nelem  = getvarsize(msurf, msoil, 'soil_moist_chg')
        !call allocr32var(r32var, nelem)
        !code = msoil%get_value('soil_moist_chg', r32var)
        !code = msurf%set_value('soil_moist_chg', r32var)

        !nelem  = getvarsize(msurf, msoil, 'soil_rechr_chg')
        !call allocr32var(r32var, nelem)
        !code = msoil%get_value('soil_rechr_chg', r32var)
        !code = msurf%set_value('soil_rechr_chg', r32var)

        nelem  = getvarsize(msurf, msoil, 'sroff')
        call allocr32var(r32var, nelem)
        code = msoil%get_value('sroff', r32var)
        code = msurf%set_value('sroff', r32var)

        !nelem  = getvarsize(msurf, msoil, 'potet')
        !call allocr32var(r32var, nelem)
        !code = msoil%get_value('potet', r32var)
        !code = msurf%set_value('potet', r32var)

        nelem  = getvarsize(msurf, msoil, 'soil_rechr')
        call allocr32var(r32var, nelem)
        code = msoil%get_value('soil_rechr', r32var)
        code = msurf%set_value('soil_rechr', r32var)

        !nelem  = getvarsize(msurf, msoil, 'soil_rechr_max')
        !call allocr32var(r32var, nelem)
        !code = msoil%get_value('soil_rechr_max', r32var)
        !code = msurf%set_value('soil_rechr_max', r32var)
        !
        nelem  = getvarsize(msurf, msoil, 'soil_moist')
        call allocr32var(r32var, nelem)
        code = msoil%get_value('soil_moist', r32var)
        code = msurf%set_value('soil_moist', r32var)

        !nelem  = getvarsize(msurf, msoil, 'soil_moist_max')
        !call allocr32var(r32var, nelem)
        !code = msoil%get_value('soil_moist_max', r32var)
        !code = msurf%set_value('soil_moist_max', r32var)
        !
        !integers
        !nelem  = getvarsize(msurf, msoil, 'srunoff_updated_soil')
        !call alloci32var(i32var, nelem)
        !code = msoil%get_value('srunoff_updated_soil', i32var)
        !code = msurf%set_value('srunoff_updated_soil', i32var)
        
    end function soil2surface
    
    function surface2soil(msurf, msoil) result(code)
        type (bmi_prms_surface), intent(in) :: msurf
        type (bmi_prms_soil), intent(inout) :: msoil
        real, allocatable, dimension(:) :: r32var
        integer, allocatable, dimension(:) :: i32var
        double precision, allocatable, dimension(:) :: r64var
        integer :: code
        integer :: gridid1,gridid2, nelem, nelem1, nelem2

        nelem  = getvarsize(msurf, msoil, 'basin_potet')
        call allocr64var(r64var, nelem)
        code = msurf%get_value('basin_potet', r64var)
        code = msoil%set_value('basin_potet', r64var)
        
        nelem  = getvarsize(msurf, msoil, 'basin_sroff')
        call allocr64var(r64var, nelem)
        code = msurf%get_value('basin_sroff', r64var)
        code = msoil%set_value('basin_sroff', r64var)

        nelem  = getvarsize(msurf, msoil, 'hru_ppt')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('hru_ppt', r32var)
        code = msoil%set_value('hru_ppt', r32var)

        nelem  = getvarsize(msurf, msoil, 'hru_area_perv')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('hru_area_perv', r32var)
        code = msoil%set_value('hru_area_perv', r32var)
        
        nelem  = getvarsize(msurf, msoil, 'hru_frac_perv')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('hru_frac_perv', r32var)
        code = msoil%set_value('hru_frac_perv', r32var)
        
        nelem  = getvarsize(msurf, msoil, 'dprst_evap_hru')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('dprst_evap_hru', r32var)
        code = msoil%set_value('dprst_evap_hru', r32var)

        nelem  = getvarsize(msurf, msoil, 'dprst_seep_hru')
        call allocr64var(r64var, nelem)
        code = msurf%get_value('dprst_seep_hru', r64var)
        code = msoil%set_value('dprst_seep_hru', r64var)

        nelem  = getvarsize(msurf, msoil, 'infil')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('infil', r32var)
        code = msoil%set_value('infil', r32var)

        nelem  = getvarsize(msurf, msoil, 'sroff')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('sroff', r32var)
        code = msoil%set_value('sroff', r32var)

        nelem  = getvarsize(msurf, msoil, 'potet')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('potet', r32var)
        code = msoil%set_value('potet', r32var)

        nelem  = getvarsize(msurf, msoil, 'transp_on')
        call alloci32var(i32var, nelem)
        code = msurf%get_value('transp_on', i32var)
        code = msoil%set_value('transp_on', i32var)

        nelem  = getvarsize(msurf, msoil, 'hru_intcpevap')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('hru_intcpevap', r32var)
        code = msoil%set_value('hru_intcpevap', r32var)

        nelem  = getvarsize(msurf, msoil, 'snow_evap')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('snow_evap', r32var)
        code = msoil%set_value('snow_evap', r32var)

        nelem  = getvarsize(msurf, msoil, 'snowcov_area')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('snowcov_area', r32var)
        code = msoil%set_value('snowcov_area', r32var)

        nelem  = getvarsize(msurf, msoil, 'soil_rechr')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('soil_rechr', r32var)
        code = msoil%set_value('soil_rechr', r32var)
        
        nelem  = getvarsize(msurf, msoil, 'soil_rechr_max')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('soil_rechr_max', r32var)
        code = msoil%set_value('soil_rechr_max', r32var)

        nelem  = getvarsize(msurf, msoil, 'soil_moist')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('soil_moist', r32var)
        code = msoil%set_value('soil_moist', r32var)

        nelem  = getvarsize(msurf, msoil, 'soil_moist_max')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('soil_moist_max', r32var)
        code = msoil%set_value('soil_moist_max', r32var)

        nelem  = getvarsize(msurf, msoil, 'hru_impervevap')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('hru_impervevap', r32var)
        code = msoil%set_value('hru_impervevap', r32var)

        nelem  = getvarsize(msurf, msoil, 'soil_moist_chg')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('soil_moist_chg', r32var)
        code = msoil%set_value('soil_moist_chg', r32var)

        nelem  = getvarsize(msurf, msoil, 'soil_rechr_chg')
        call allocr32var(r32var, nelem)
        code = msurf%get_value('soil_rechr_chg', r32var)
        code = msoil%set_value('soil_rechr_chg', r32var)

        nelem  = getvarsize(msurf, msoil, 'srunoff_updated_soil')
        call alloci32var(i32var, nelem)
        code = msurf%get_value('srunoff_updated_soil', i32var)
        code = msoil%set_value('srunoff_updated_soil', i32var)

        !nelem  = getvarsize(msurf, msoil, 'g2sm_grav')
        !call allocr32var(r32var, nelem)
        !code = msurf%get_value('g2sm_grav', r32var)
        !code = msoil%set_value('g2sm_grav', r32var)

        !nelem  = getvarsize(msurf, msoil, 'seg_gwflow')
        !call allocr64var(r64var, nelem)
        !code = msurf%get_value('seg_gwflow', r64var)
        !code = msoil%set_value('seg_gwflow', r64var)
        !
        !nelem  = getvarsize(msurf, msoil, 'seg_inflow')
        !call allocr64var(r64var, nelem)
        !code = msurf%get_value('seg_inflow', r64var)
        !code = msoil%set_value('seg_inflow', r64var)
        !
        !nelem  = getvarsize(msurf, msoil, 'seg_outflow')
        !call allocr64var(r64var, nelem)
        !code = msurf%get_value('seg_outflow', r64var)
        !code = msoil%set_value('seg_outflow', r64var)

        nelem  = getvarsize(msurf, msoil, 'strm_seg_in')
        call allocr64var(r64var, nelem)
        code = msurf%get_value('strm_seg_in', r64var)
        code = msoil%set_value('strm_seg_in', r64var)
        
    end function surface2soil
    
    function getvarsize(msurf, msoil, vname) result(size)
        type (bmi_prms_surface), intent(in) :: msurf
        type (bmi_prms_soil), intent(in) :: msoil
        character(len=*), intent(in) :: vname
        integer :: gridid1, gridid2
        integer :: size, nelem1, nelem2
        integer code
        code = msurf%get_var_grid(vname, gridid1)
        code = msoil%get_var_grid(vname, gridid2)
        code = msurf%get_grid_size(gridid1, nelem1)
        code = msoil%get_grid_size(gridid2, nelem2)
        if(nelem1.ne.nelem2) then
            write(*,*) 'not equal number of hrus'
            stop BMI_FAILURE
        else
            size = nelem1
        endif
    end function getvarsize
    
    subroutine allocr64var(var, size)
        double precision, allocatable, intent(inout) :: var(:)
        integer, intent(in) :: size
        if (allocated(var)) deallocate(var)
        allocate(var(size))
        var = 0.0d0
    end subroutine
        
    subroutine allocr32var(var, size)
        real, allocatable, intent(inout) :: var(:)
        integer, intent(in) :: size
        if (allocated(var)) deallocate(var)
        allocate(var(size))
        var = 0.0
    end subroutine

    subroutine alloci32var(var, size)
        integer, allocatable, intent(inout) :: var(:)
        integer, intent(in) :: size
        if (allocated(var)) deallocate(var)
        allocate(var(size))
        var = 0
    end subroutine

end program irf_test
