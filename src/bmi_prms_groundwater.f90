    module bmiprmsgroundwater

    use m_prms_groundwater
    use bmif_2_0
    use, intrinsic :: iso_c_binding, only: c_ptr, c_loc, c_f_pointer
    implicit none

    type, extends (bmi) :: bmi_prms_groundwater
        private
        type (prms_groundwater_model) :: model
    contains
    procedure :: get_component_name => prms_component_name
    procedure :: get_input_item_count => prms_input_item_count
    procedure :: get_output_item_count => prms_output_item_count
    procedure :: get_input_var_names => prms_input_var_names
    procedure :: get_output_var_names => prms_output_var_names
    procedure :: initialize => prms_initialize
    procedure :: finalize => prms_finalize
    procedure :: get_start_time => prms_start_time
    procedure :: get_end_time => prms_end_time
    procedure :: get_current_time => prms_current_time
    procedure :: get_time_step => prms_time_step
    procedure :: get_time_units => prms_time_units
    procedure :: update => prms_update
    procedure :: update_until => prms_update_until
    procedure :: get_var_grid => prms_var_grid
    procedure :: get_grid_type => prms_grid_type
    procedure :: get_grid_rank => prms_grid_rank
    procedure :: get_grid_shape => prms_grid_shape
    procedure :: get_grid_size => prms_grid_size
    procedure :: get_grid_spacing => prms_grid_spacing
    procedure :: get_grid_origin => prms_grid_origin
    procedure :: get_grid_x => prms_grid_x
    procedure :: get_grid_y => prms_grid_y
    procedure :: get_grid_z => prms_grid_z
    procedure :: get_grid_node_count => prms_grid_node_count
    procedure :: get_grid_edge_count => prms_grid_edge_count
    procedure :: get_grid_face_count => prms_grid_face_count
    procedure :: get_grid_edge_nodes => prms_grid_edge_nodes
    procedure :: get_grid_face_edges => prms_grid_face_edges
    procedure :: get_grid_face_nodes => prms_grid_face_nodes
    procedure :: get_grid_nodes_per_face => prms_grid_nodes_per_face
    procedure :: get_var_type => prms_var_type
    procedure :: get_var_units => prms_var_units
    procedure :: get_var_itemsize => prms_var_itemsize
    procedure :: get_var_nbytes => prms_var_nbytes
    procedure :: get_var_location => prms_var_location
    procedure :: get_value_int => prms_get_int
    procedure :: get_value_float => prms_get_float
    procedure :: get_value_double => prms_get_double
    generic :: get_value => &
         get_value_int, &
         get_value_float, &
         get_value_double
    procedure :: get_value_ptr_int => prms_get_ptr_int
    procedure :: get_value_ptr_float => prms_get_ptr_float
    procedure :: get_value_ptr_double => prms_get_ptr_double
    generic :: get_value_ptr => &
         get_value_ptr_int, &
         get_value_ptr_float, &
         get_value_ptr_double
    procedure :: get_value_at_indices_int => prms_get_at_indices_int
    procedure :: get_value_at_indices_float => prms_get_at_indices_float
    procedure :: get_value_at_indices_double => prms_get_at_indices_double
    generic :: get_value_at_indices => &
         get_value_at_indices_int, &
         get_value_at_indices_float, &
         get_value_at_indices_double
    procedure :: set_value_int => prms_set_int
    procedure :: set_value_float => prms_set_float
    procedure :: set_value_double => prms_set_double
    generic :: set_value => &
         set_value_int, &
         set_value_float, &
         set_value_double
    procedure :: set_value_at_indices_int => prms_set_at_indices_int
    procedure :: set_value_at_indices_float => prms_set_at_indices_float
    procedure :: set_value_at_indices_double => prms_set_at_indices_double
    generic :: set_value_at_indices => &
         set_value_at_indices_int, &
         set_value_at_indices_float, &
         set_value_at_indices_double
    !procedure :: print_model_info
    end type bmi_prms_groundwater

    private
    public :: bmi_prms_groundwater

    character (len=BMI_MAX_COMPONENT_NAME), target :: &
        component_name = "prms6-groundwater-BMI"

    ! Exchange items
    integer, parameter :: input_item_count = 15
    integer, parameter :: output_item_count = 14
    character (len=BMI_MAX_VAR_NAME), target, &
        dimension(input_item_count) :: input_items = (/ &
        
    !vars below required input to groundwater module from surface and soil modules
    !vars from climate
    'pkwater_equiv     ', & !r64 by nhru
    
    !vars from intcp
    'hru_intcpstor     ', & !r32 by nhru
    
    !vars from soil
    'soil_moist_tot    ', & !r32 by nhru
    'soil_to_gw        ', & !r32 by nhru
    'ssr_to_gw         ', & !r32 by nhru
    'ssres_flow        ', & !r32 by nhru
    
    !vars from runoff
    'dprst_seep_hru    ', & !r64  by nhru
    'dprst_stor_hru    ', & !r64 by nhru
    'hru_impervstor    ', & !r32 by nhru
    'sroff             ', & !r32 by nhru
        !input vars that could be used in calibration
    'gwsink_coef       ', & !r32 by ngw
    'gwflow_coef       ', & !r32 by ngw
    'gwres_flow        ', & !r32 by nhru
    'gwres_sink        ', & !r32 by nhru
    'gwres_stor        ' & !r64 by nhru
    /) 
    character (len=BMI_MAX_VAR_NAME), target, &
        dimension(output_item_count) :: output_items = (/ &
        !vars required for streamflow module from this(groundwater)
    'gwres_flow        ', & !r32 by nhru
        !output vars used in calibration
    'gwsink_coef       ', & !r32 by ngw
    'gflow_coef        ', & !r32 by ngw
        !output vars used in water-balanc module
    'gwin_dprst        ', & !r64 by nhru
    'gwres_in          ', & !r64 by nhru
    'gwres_stor        ', & !r64 by nhru
    'gwres_stor_ante   ', & !r64 by nhru
    'gwstor_minarea_wb ', & !r64 by nhru
    'gw_upslope        ', & !r64 by nhru
    'hru_gw_cascadeflow', & !r32 by nhru
    'hru_storage       ', & !r64 by nhru
    'hru_storage_ante  ', & !r64 by nhru
    'gwres_sink        ', & !r32 by nhru
    'has_gwstor_minarea' & !logical by 1
    /) 

    contains

    ! Get the name of the model.
    function prms_component_name(this, name) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (len=*), pointer, intent(out) :: name
    integer :: bmi_status

    name => component_name
    bmi_status = BMI_SUCCESS
    end function prms_component_name

    ! Count the input variables.
    function prms_input_item_count(this, count) result (bmi_status)
        class (bmi_prms_groundwater), intent(in) :: this
        integer, intent(out) :: count
        integer :: bmi_status

        count = input_item_count
        bmi_status = BMI_SUCCESS
     end function prms_input_item_count

    ! Count the output variables.
    function prms_output_item_count(this, count) result (bmi_status)
        class (bmi_prms_groundwater), intent(in) :: this
        integer, intent(out) :: count
        integer :: bmi_status

        count = output_item_count
        bmi_status = BMI_SUCCESS
    end function prms_output_item_count

    ! List input variables.
    function prms_input_var_names(this, names) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status
    
    names => input_items
    bmi_status = BMI_SUCCESS
    end function prms_input_var_names

    ! List output variables.
    function prms_output_var_names(this, names) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (*), pointer, intent(out) :: names(:)
    integer :: bmi_status
    names => output_items
    bmi_status = BMI_SUCCESS
    end function prms_output_var_names

    ! BMI initializer.
    function prms_initialize(this, config_file) result (bmi_status)
    class (bmi_prms_groundwater), intent(out) :: this
    character (len=*), intent(in) :: config_file
    integer :: bmi_status

    if (len(config_file) > 0) then
        call initialize_from_file(this%model, config_file)
    else
        !call initialize_from_defaults(this%model)
    end if
    bmi_status = BMI_SUCCESS
    end function prms_initialize

    ! BMI finalizer.
    function prms_finalize(this) result (bmi_status)
    class (bmi_prms_groundwater), intent(inout) :: this
    integer :: bmi_status

    call cleanup(this%model)
    bmi_status = BMI_SUCCESS
    end function prms_finalize

    ! Model start time.
    function prms_start_time(this, time) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = 0.d0
    !time = this%model%model_simulation%model_time%Timestep
    bmi_status = BMI_SUCCESS
    end function prms_start_time

    ! Model end time.
    function prms_end_time(this, time) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%model_simulation%model_time%Number_timesteps)
    bmi_status = BMI_SUCCESS
    end function prms_end_time

    ! Model current time.
    function prms_current_time(this, time) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    double precision, intent(out) :: time
    integer :: bmi_status

    time = dble(this%model%model_simulation%model_time%Timestep)
    bmi_status = BMI_SUCCESS
    end function prms_current_time

    ! Model time step.
    function prms_time_step(this, time_step) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    double precision, intent(out) :: time_step
    integer :: bmi_status

    time_step = dble(this%model%model_simulation%model_time%Timestep_seconds)
    bmi_status = BMI_SUCCESS
    end function prms_time_step

    ! Model time units.
    function prms_time_units(this, units) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (len=*), intent(out) :: units
    integer :: bmi_status

    units = "s"
    bmi_status = BMI_SUCCESS
    end function prms_time_units

    ! Advance model by one time step.
    function prms_update(this) result (bmi_status)
    class (bmi_prms_groundwater), intent(inout) :: this
    integer :: bmi_status

    call advance_in_time(this%model)
    bmi_status = BMI_SUCCESS
    end function prms_update

    ! Advance the model until the given time.
    function prms_update_until(this, time) result (bmi_status)
    class (bmi_prms_groundwater), intent(inout) :: this
    double precision, intent(in) :: time
    double precision :: current_time, end_time, dt
    integer :: bmi_status
    double precision :: n_steps_real
    integer :: n_steps, i, s
    s = this%get_current_time(current_time)
    s = this%get_end_time(end_time)
    s = this%get_time_step(dt)
    if (time > current_time) then
        n_steps_real = (time - current_time)
        n_steps = floor(n_steps_real)
        do i = 1, n_steps
            s = this%update()
        end do
        !s = this%update_frac(n_steps_real - dble(n_steps))
    end if
    bmi_status = BMI_SUCCESS
    end function prms_update_until

    ! Get the grid id for a particular variable.
    function prms_var_grid(this, name, grid) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, intent(out) :: grid
    integer :: bmi_status

    select case(name)
    case('pkwater_equiv', 'hru_intcpstor', 'soil_moist_tot', &
        'soil_to_gw', 'ssr_to_gw', 'ssres_flow', 'dprst_seep_hru', &
        'dprst_stor_hru', 'hru_impervstor', 'sroff', 'gwres_flow', &
        'gwres_sink', 'gwres_stor', 'gwin_dprst', 'gwres_in', &
        'gwres_stor_ante', 'gwstor_minarea_wb', 'gw_upslope', &
        'hru_gw_cascadeflow', 'hru_storage', 'hru_storage_ante')
        grid = 0
        bmi_status = BMI_SUCCESS
    case('gwsink_coef', 'gwflow_coef')
        grid = 1
        bmi_status = BMI_SUCCESS
        case default
        grid = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_grid

    ! The type of a variable's grid.
    function prms_grid_type(this, grid, type) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    integer, intent(in) :: grid
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(grid)
    case(0)
        type = "vector"
        bmi_status = BMI_SUCCESS
     case(1)
        type = "vector"
        bmi_status = BMI_SUCCESS
    case default
        type = "-"
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_type

    ! The number of dimensions of a grid.
    function prms_grid_rank(this, grid, rank) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: rank
    integer :: bmi_status

    select case(grid)
    case(0)
        rank = 1
        bmi_status = BMI_SUCCESS
    case(1)
        rank = 1
        bmi_status = BMI_SUCCESS
    case default
        rank = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_rank

    ! The dimensions of a grid.
    function prms_grid_shape(this, grid, shape) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    integer, intent(in) :: grid
    integer, dimension(:), intent(out) :: shape
    integer :: bmi_status
    
    select case(grid)
    case(0)
        shape(:) = [this%model%model_simulation%model_basin%nhru]
        bmi_status = BMI_SUCCESS
    case(1)
        shape(:) = [this%model%model_simulation%groundwater%ngw]
    case default
        shape(:) = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_shape
    
    ! The total number of elements in a grid.
    function prms_grid_size(this, grid, size) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    integer, intent(in) :: grid
    integer, intent(out) :: size
    integer :: bmi_status

    select case(grid)
    case(0)
        size = this%model%model_simulation%model_basin%nhru
        bmi_status = BMI_SUCCESS
    case(1)
        size = this%model%model_simulation%groundwater%ngw
        bmi_status = BMI_SUCCESS
    case default
        size = -1
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_size

    ! The distance between nodes of a grid.
    function prms_grid_spacing(this, grid, spacing) result (bmi_status)
     class (bmi_prms_groundwater), intent(in) :: this
     integer, intent(in) :: grid
     double precision, dimension(:), intent(out) :: spacing
     integer :: bmi_status
    
     select case(grid)
     case default
        spacing(:) = -1.d0
        bmi_status = BMI_FAILURE
     end select
    end function prms_grid_spacing
    
    ! Coordinates of grid origin.
    function prms_grid_origin(this, grid, origin) result (bmi_status)
     class (bmi_prms_groundwater), intent(in) :: this
     integer, intent(in) :: grid
     double precision, dimension(:), intent(out) :: origin
     integer :: bmi_status
    
     select case(grid)
     case default
        origin(:) = -1.d0
        bmi_status = BMI_FAILURE
     end select
    end function prms_grid_origin
    
    ! X-coordinates of grid nodes.
    function prms_grid_x(this, grid, x) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: x
    integer :: bmi_status

    select case(grid)
    case(0:1)
        x = this%model%model_simulation%model_basin%hru_x
        bmi_status = BMI_SUCCESS
        case default
        x(:) = -1.d0
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_x

    ! Y-coordinates of grid nodes.
    function prms_grid_y(this, grid, y) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: y
    integer :: bmi_status

    select case(grid)
    case(0:1)
        y = this%model%model_simulation%model_basin%hru_y
        bmi_status = BMI_SUCCESS
        case default
        y(:) = -1.d0
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_y

    ! Z-coordinates of grid nodes.
    function prms_grid_z(this, grid, z) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    integer, intent(in) :: grid
    double precision, dimension(:), intent(out) :: z
    integer :: bmi_status

    select case(grid)
    case(0:1)
        z = this%model%model_simulation%model_basin%hru_elev
        bmi_status = BMI_SUCCESS
        case default
        z(:) = -1.d0
        bmi_status = BMI_FAILURE
    end select
    end function prms_grid_z
    
    ! Get the number of nodes in an unstructured grid.
    function prms_grid_node_count(this, grid, count) result(bmi_status)
      class(bmi_prms_groundwater), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status

      select case(grid)
      case(0:1)
         bmi_status = this%get_grid_size(grid, count)
      case default
         count = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_node_count

    ! Get the number of edges in an unstructured grid.
    function prms_grid_edge_count(this, grid, count) result(bmi_status)
      class(bmi_prms_groundwater), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status

      select case(grid)
      case (0:1)
         bmi_status = this%get_grid_node_count(grid, count)
         count = count - 1
      case default
         count = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_edge_count

    ! Get the number of faces in an unstructured grid.
    function prms_grid_face_count(this, grid, count) result(bmi_status)
      class(bmi_prms_groundwater), intent(in) :: this
      integer, intent(in) :: grid
      integer, intent(out) :: count
      integer :: bmi_status

      select case(grid)
      case (0:1)
         count = 0
         bmi_status = BMI_SUCCESS
      case default
         count = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_face_count

    ! Get the edge-node connectivity.
    function prms_grid_edge_nodes(this, grid, edge_nodes) result(bmi_status)
      class(bmi_prms_groundwater), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: edge_nodes
      integer :: bmi_status

      select case(grid)
      case default
         edge_nodes(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_edge_nodes

    ! Get the face-edge connectivity.
    function prms_grid_face_edges(this, grid, face_edges) result(bmi_status)
      class(bmi_prms_groundwater), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: face_edges
      integer :: bmi_status

      select case(grid)
      case default
         face_edges(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_face_edges

    ! Get the face-node connectivity.
    function prms_grid_face_nodes(this, grid, face_nodes) result(bmi_status)
      class(bmi_prms_groundwater), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: face_nodes
      integer :: bmi_status

      select case(grid)
      case default
         face_nodes(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_face_nodes

    ! Get the number of nodes for each face.
    function prms_grid_nodes_per_face(this, grid, nodes_per_face) result(bmi_status)
      class(bmi_prms_groundwater), intent(in) :: this
      integer, intent(in) :: grid
      integer, dimension(:), intent(out) :: nodes_per_face
      integer :: bmi_status

      select case(grid)
      case default
         nodes_per_face(:) = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_grid_nodes_per_face

    ! The data type of the variable, as a string.
    function prms_var_type(this, name, type) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: type
    integer :: bmi_status

    select case(name)
    case('hru_intcpstor', 'soil_moist_tot', 'soil_to_gw', &
        'ssr_to_gw', 'ssres_flow', 'hru_impervstor', 'sroff', 'gwres_flow', &
        'gwsink_coef', 'gwflow_coef', 'gwres_sink', 'hru_gw_cascadeflow')
        type = "real"
        bmi_status = BMI_SUCCESS
    case('pkwater_equiv',  'dprst_seep_hru', 'dprst_stor_hru', &
        'gwin_dprst', 'gwres_in', 'gwres_stor', 'gwres_stor_ante', 'gwstor_minarea_wb', &
        'gw_upslope', 'hru_storage', 'hru_storage_ante')
        type = "double precision"
        bmi_status = BMI_SUCCESS
    case('has_gwstor_minarea')
        type = 'logical'
        bmi_status = BMI_SUCCESS
    case default
        type = "-"
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_type

    ! The units of the given variable.
    function prms_var_units(this, name, units) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (len=*), intent(in) :: name
    character (len=*), intent(out) :: units
    integer :: bmi_status

    select case(name)
    case('pkwater_equiv', 'hru_intcpstor', 'soil_moist_tot', &
        'soil_to_gw', 'ssr_to_gw', 'ssres_flow', 'dprst_seep_hru', &
        'dprst_stor_hru', 'hru_impervstor', 'sroff', 'gwres_flow', &
        'gwres_sink', 'gwres_stor', 'gwstor_minarea_wb', &
        'hru_gw_cascadeflow', 'hru_storage', 'gwin_dprst', &
        'gwres_stor_ante', 'hru_storage_ante')
        units = "in"
        bmi_status = BMI_SUCCESS
    case('gwres_in', 'gw_upslope')
        units = 'acre-inches'
        bmi_status = BMI_SUCCESS
    case('gwsink_coef', 'gwflow_coef')
        units = 'fraction/day'
        bmi_status = BMI_SUCCESS
    case default
        units = "-"
        bmi_status = BMI_FAILURE
    end select
    end function prms_var_units

    ! Memory use per array element.
    function prms_var_itemsize(this, name, size) result (bmi_status)
      class (bmi_prms_groundwater), intent(in) :: this
      character (len=*), intent(in) :: name
      integer, intent(out) :: size
      integer :: bmi_status
    
      select case(name)
      case("pkwater_equiv")
         size = sizeof(this%model%model_simulation%climate%pkwater_equiv(1))
         bmi_status = BMI_SUCCESS
      case("hru_intcpstor")
         size = sizeof(this%model%model_simulation%intcp%hru_intcpstor(1))
         bmi_status = BMI_SUCCESS
      case("soil_moist_tot")
         size = sizeof(this%model%model_simulation%soil%soil_moist_tot(1))
         bmi_status = BMI_SUCCESS
      case("soil_to_gw")
         size = sizeof(this%model%model_simulation%soil%soil_to_gw(1))
         bmi_status = BMI_SUCCESS
      case("ssr_to_gw")
         size = sizeof(this%model%model_simulation%soil%ssr_to_gw(1))
         bmi_status = BMI_SUCCESS
      case("ssres_flow")
         size = sizeof(this%model%model_simulation%soil%ssres_flow(1))
         bmi_status = BMI_SUCCESS
      case("dprst_seep_hru")
         size = sizeof(this%model%model_simulation%runoff%dprst_seep_hru(1))
         bmi_status = BMI_SUCCESS
      case("dprst_stor_hru")
         size = sizeof(this%model%model_simulation%runoff%dprst_stor_hru(1))
         bmi_status = BMI_SUCCESS
      case("hru_impervstor")
         size = sizeof(this%model%model_simulation%runoff%hru_impervstor(1))
         bmi_status = BMI_SUCCESS
      case("sroff")
         size = sizeof(this%model%model_simulation%runoff%sroff(1))
         bmi_status = BMI_SUCCESS
      case("gwres_flow")
         size = sizeof(this%model%model_simulation%groundwater%gwres_flow(1))
         bmi_status = BMI_SUCCESS
      case('gwsink_coef')
        size = sizeof(this%model%model_simulation%groundwater%gwsink_coef(1)) 
        bmi_status = BMI_SUCCESS
      case('gwflow_coef')
        size = sizeof(this%model%model_simulation%groundwater%gwflow_coef(1)) 
        bmi_status = BMI_SUCCESS
      case('gwin_dprst')
        size = sizeof(this%model%model_simulation%groundwater%gwin_dprst(1)) 
        bmi_status = BMI_SUCCESS
      case('has_gwstor_minarea')
        size = sizeof(this%model%model_simulation%groundwater%has_gwstor_minarea) 
        bmi_status = BMI_SUCCESS
      case('gwres_in')
        size = sizeof(this%model%model_simulation%groundwater%gwres_in(1)) 
        bmi_status = BMI_SUCCESS
      case('gwres_sink')
        size = sizeof(this%model%model_simulation%groundwater%gwres_sink(1)) 
        bmi_status = BMI_SUCCESS
      case('gwres_stor')
        size = sizeof(this%model%model_simulation%groundwater%gwres_stor(1)) 
        bmi_status = BMI_SUCCESS
      case('gwres_stor_ante')
        size = sizeof(this%model%model_simulation%groundwater%gwres_stor_ante(1)) 
        bmi_status = BMI_SUCCESS
      case('gwstor_minarea_wb')
        size = sizeof(this%model%model_simulation%groundwater%gwstor_minarea_wb(1)) 
        bmi_status = BMI_SUCCESS
      case('gw_upslope')
        if(this%model%control_data%cascadegw_flag%value > 0) then 
            size = sizeof(this%model%model_simulation%groundwater%gw_upslope(1)) 
            bmi_status = BMI_SUCCESS
        else
            size = -1
            bmi_status = BMI_FAILURE
        endif
      case('hru_gw_cascadeflow')
        if(this%model%control_data%cascadegw_flag%value > 0) then 
            size = sizeof(this%model%model_simulation%groundwater%hru_gw_cascadeflow(1)) 
            bmi_status = BMI_SUCCESS
        else
            size = -1
            bmi_status = BMI_FAILURE
        endif
      case('hru_storage')
        size = sizeof(this%model%model_simulation%groundwater%hru_storage(1)) 
        bmi_status = BMI_SUCCESS
      case('hru_storage_ante')
        size = sizeof(this%model%model_simulation%groundwater%hru_storage_ante(1)) 
        bmi_status = BMI_SUCCESS
      case default
         size = -1
         bmi_status = BMI_FAILURE
      end select
    end function prms_var_itemsize
    
    ! The size of the given variable.
    function prms_var_nbytes(this, name, nbytes) result (bmi_status)
      class (bmi_prms_groundwater), intent(in) :: this
      character (len=*), intent(in) :: name
      integer, intent(out) :: nbytes
      integer :: bmi_status
      integer :: s1, s2, s3, type, grid_size, item_size
    
      s1 = this%get_var_grid(name, type)
      s2 = this%get_grid_size(type, grid_size)
      s3 = this%get_var_itemsize(name, item_size)
    
      if ((s1 == BMI_SUCCESS).and.(s2 == BMI_SUCCESS).and.(s3 == BMI_SUCCESS)) then
         nbytes = item_size * grid_size
         bmi_status = BMI_SUCCESS
      else
         nbytes = -1
         bmi_status = BMI_FAILURE
      end if
    end function prms_var_nbytes
    
  ! The location (node, face, edge) of the given variable.
    function prms_var_location(this, name, location) result (bmi_status)
        class (bmi_prms_groundwater), intent(in) :: this
        character (len=*), intent(in) :: name
        character (len=*), intent(out) :: location
        integer :: bmi_status

        select case(name)
        case default
           location = "face"
           bmi_status = BMI_SUCCESS
        end select
    end function prms_var_location
    ! Get a copy of a integer variable's values, flattened.
    function prms_get_int(this, name, dest) result (bmi_status)
      class (bmi_prms_groundwater), intent(in) :: this
      character (len=*), intent(in) :: name
      integer, intent(inout) :: dest(:)
      integer :: bmi_status
    
      select case(name)
      case('has_gwstor_minarea')
         dest = [this%model%model_simulation%groundwater%has_gwstor_minarea]
         bmi_status = BMI_SUCCESS
      case default
         dest = [-1]
         bmi_status = BMI_FAILURE
      end select
    end function prms_get_int
    
    ! Get a copy of a real variable's values, flattened.
    function prms_get_float(this, name, dest) result (bmi_status)
      class (bmi_prms_groundwater), intent(in) :: this
      character (len=*), intent(in) :: name
      real, intent(inout) :: dest(:)
      integer :: bmi_status
    
      select case(name)
      case('gwres_flow')
          dest = [this%model%model_simulation%groundwater%gwres_flow]
          bmi_status = BMI_SUCCESS
      case('gwres_sink')
          dest = [this%model%model_simulation%groundwater%gwres_sink]
          bmi_status = BMI_SUCCESS
      case('hru_gw_cascadeflow')
        if(this%model%control_data%cascadegw_flag%value > 0) then 
            dest = [this%model%model_simulation%groundwater%hru_gw_cascadeflow]
            bmi_status = BMI_SUCCESS
        else
            dest = [-1.d0]
            bmi_status = BMI_FAILURE
        endif
      case default
         dest = [-1.0]
         bmi_status = BMI_FAILURE
      end select
    end function prms_get_float
    
    ! Get a copy of a double variable's values, flattened.
    function prms_get_double(this, name, dest) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer :: bmi_status
    
    select case(name)
    case('gwin_dprst')
        dest = [this%model%model_simulation%groundwater%gwin_dprst]
        bmi_status = BMI_SUCCESS
    case('gwres_in')
        dest = [this%model%model_simulation%groundwater%gwres_in]
        bmi_status = BMI_SUCCESS
    case('gwres_stor')
        dest = [this%model%model_simulation%groundwater%gwres_stor]
        bmi_status = BMI_SUCCESS
    case('gwres_stor_ante')
        dest = [this%model%model_simulation%groundwater%gwres_stor_ante]
        bmi_status = BMI_SUCCESS
    case('gwstor_minarea_wb')
        dest = [this%model%model_simulation%groundwater%gwstor_minarea_wb]
        bmi_status = BMI_SUCCESS
    case('gw_upslope')
        if(this%model%control_data%cascadegw_flag%value > 0) then 
            dest = [this%model%model_simulation%groundwater%gw_upslope]
            bmi_status = BMI_SUCCESS
        else
            dest = [-1.d0]
            bmi_status = BMI_FAILURE
        endif
    case('hru_storage')
            dest = [this%model%model_simulation%groundwater%hru_storage]
            bmi_status = BMI_SUCCESS
    case('hru_storage_ante')
        dest = [this%model%model_simulation%groundwater%hru_storage_ante]
        bmi_status = BMI_SUCCESS
    case default
        dest = [-1.d0]
        bmi_status = BMI_FAILURE
      end select
    end function prms_get_double
    
    function prms_get_ptr_int(this, name, dest_ptr) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (len=*), intent(in) :: name
    integer, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status, status
    type (c_ptr) :: src
    integer :: n_elements, gridid
        
    status = this%get_var_grid(name,gridid)
    status = this%get_grid_size(gridid, n_elements)

    select case(name)
    !case('nowtime')
    !    src = c_loc(this%model%model_simulation%model_time%nowtime(1))
    !    call c_f_pointer(src, dest_ptr, [n_elements])
    !    bmi_status = BMI_SUCCESS
    case default
       bmi_status = BMI_FAILURE
    end select

    end function prms_get_ptr_int
    !
    !! Get a reference to a real-valued variable, flattened.
    function prms_get_ptr_float(this, name, dest_ptr) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (len=*), intent(in) :: name
    real, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements, gridid, status
        
    status = this%get_var_grid(name,gridid)
    status = this%get_grid_size(gridid, n_elements)

    select case(name)
    case('gwres_flow')
        src = c_loc(this%model%model_simulation%groundwater%gwres_flow(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('gwres_sink')
        src = c_loc(this%model%model_simulation%groundwater%gwres_sink(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_gw_cascadeflow')
        if(this%model%control_data%cascadegw_flag%value > 0) then 
            src = c_loc(this%model%model_simulation%groundwater%hru_gw_cascadeflow(1))
            call c_f_pointer(src, dest_ptr, [n_elements])
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case default
    
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_ptr_float
    !
    !! Get a reference to an double-valued variable, flattened.
    function prms_get_ptr_double(this, name, dest_ptr) result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, pointer, intent(inout) :: dest_ptr(:)
    integer :: bmi_status
    type (c_ptr) :: src
    integer :: n_elements, status, gridid
    
    status = this%get_var_grid(name,gridid)
    status = this%get_grid_size(gridid, n_elements)

    select case(name)
        !value below dimed by 1 not really necessary to get ptr?
    case('gwin_dprst')
        src = c_loc(this%model%model_simulation%groundwater%gwin_dprst(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('gwres_in')
        src = c_loc(this%model%model_simulation%groundwater%gwres_in(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('gwres_stor')
        src = c_loc(this%model%model_simulation%groundwater%gwres_stor(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('gwres_stor_ante')
        src = c_loc(this%model%model_simulation%groundwater%gwres_stor_ante(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('gwstor_minarea_wb')
        src = c_loc(this%model%model_simulation%groundwater%gwstor_minarea_wb(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('gw_upslope')
        if(this%model%control_data%cascadegw_flag%value > 0) then 
            src = c_loc(this%model%model_simulation%groundwater%gw_upslope(1))
            call c_f_pointer(src, dest_ptr, [n_elements])
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case('hru_storage')
        src = c_loc(this%model%model_simulation%groundwater%hru_storage(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case('hru_storage_ante')
        src = c_loc(this%model%model_simulation%groundwater%hru_storage_ante(1))
        call c_f_pointer(src, dest_ptr, [n_elements])
        bmi_status = BMI_SUCCESS
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_ptr_double
    
    ! Get values of an integer variable at the given locations.
    function prms_get_at_indices_int(this, name, dest, inds) &
         result (bmi_status)
      class (bmi_prms_groundwater), intent(in) :: this
      character (len=*), intent(in) :: name
      integer, intent(inout) :: dest(:)
      integer, intent(in) :: inds(:)
      integer :: bmi_status
      type (c_ptr) src
      integer, pointer :: src_flattened(:)
      integer :: i, n_elements, status, gridid
    
      select case(name)
      case default
         bmi_status = BMI_FAILURE
      end select
    end function prms_get_at_indices_int
    
    ! Get values of a real variable at the given locations.
    function prms_get_at_indices_float(this, name, dest, inds) &
         result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (len=*), intent(in) :: name
    real, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    real, pointer :: src_flattened(:)
    integer :: i, n_elements, status, gridid
        
    status = this%get_var_grid(name,gridid)
    status = this%get_grid_size(gridid, n_elements)

    select case(name)
    case('gwres_flow')
        src = c_loc(this%model%model_simulation%groundwater%gwres_flow(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('gwres_sink')
        src = c_loc(this%model%model_simulation%groundwater%gwres_sink(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_gw_cascadeflow')
        if(this%model%control_data%cascadegw_flag%value > 0) then 
            src = c_loc(this%model%model_simulation%groundwater%hru_gw_cascadeflow(1))
            call c_f_pointer(src, src_flattened, [n_elements])
            do i = 1,  size(inds)
                dest(i) = src_flattened(inds(i))
            end do
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_at_indices_float
    
    ! Get values of a double variable at the given locations.
    function prms_get_at_indices_double(this, name, dest, inds) &
         result (bmi_status)
    class (bmi_prms_groundwater), intent(in) :: this
    character (len=*), intent(in) :: name
    double precision, intent(inout) :: dest(:)
    integer, intent(in) :: inds(:)
    integer :: bmi_status
    type (c_ptr) src
    double precision, pointer :: src_flattened(:)
    integer :: i, n_elements, status, gridid
        
    status = this%get_var_grid(name,gridid)
    status = this%get_grid_size(gridid, n_elements)

    select case(name)
    case('gwin_dprst')
        src = c_loc(this%model%model_simulation%groundwater%gwin_dprst(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('gwres_in')
        src = c_loc(this%model%model_simulation%groundwater%gwres_in(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('gwres_stor')
        src = c_loc(this%model%model_simulation%groundwater%gwres_stor(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('gwres_stor_ante')
        src = c_loc(this%model%model_simulation%groundwater%gwres_stor_ante(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('gwstor_minarea_wb')
        src = c_loc(this%model%model_simulation%groundwater%gwstor_minarea_wb(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('gw_upslope')
        if(this%model%control_data%cascadegw_flag%value > 0) then 
        src = c_loc(this%model%model_simulation%groundwater%gw_upslope(1))
            call c_f_pointer(src, src_flattened, [n_elements])
            do i = 1,  size(inds)
                dest(i) = src_flattened(inds(i))
            end do
            bmi_status = BMI_SUCCESS
        else
            bmi_status = BMI_FAILURE
        endif
    case('hru_storage')
        src = c_loc(this%model%model_simulation%groundwater%hru_storage(1))
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
    case('hru_storage_ante')
        call c_f_pointer(src, src_flattened, [n_elements])
        do i = 1,  size(inds)
            dest(i) = src_flattened(inds(i))
        end do
        bmi_status = BMI_SUCCESS
        case default
        bmi_status = BMI_FAILURE
    end select
    end function prms_get_at_indices_double
    
    ! Set new integer values.
    function prms_set_int(this, name, src) result (bmi_status)
      class (bmi_prms_groundwater), intent(inout) :: this
      character (len=*), intent(in) :: name
      integer, intent(in) :: src(:)
      integer :: bmi_status
    
      select case(name)
      !case("model__identification_number")
      !   this%model%id = src(1)
      !   bmi_status = BMI_SUCCESS
      case default
         bmi_status = BMI_FAILURE
      end select
    end function prms_set_int
    
    ! Set new real values.
    function prms_set_float(this, name, src) result (bmi_status)
      class (bmi_prms_groundwater), intent(inout) :: this
      character (len=*), intent(in) :: name
      real, intent(in) :: src(:)
      integer :: bmi_status
    
      select case(name)
      case("hru_intcpstor")
         this%model%model_simulation%intcp%hru_intcpstor = src
         bmi_status = BMI_SUCCESS
      case("soil_moist_tot")
         this%model%model_simulation%soil%soil_moist_tot = src
         bmi_status = BMI_SUCCESS
      case("soil_to_gw")
         this%model%model_simulation%soil%soil_to_gw = src
         bmi_status = BMI_SUCCESS
      case("ssr_to_gw")
         this%model%model_simulation%soil%ssr_to_gw = src
         bmi_status = BMI_SUCCESS
      case("ssres_flow")
         this%model%model_simulation%soil%ssres_flow = src
         bmi_status = BMI_SUCCESS
      case("hru_impervstor")
         this%model%model_simulation%runoff%hru_impervstor = src
         bmi_status = BMI_SUCCESS
      case("sroff")
         this%model%model_simulation%runoff%sroff = src
         bmi_status = BMI_SUCCESS
      case("gwres_flow")
         this%model%model_simulation%groundwater%gwres_flow = src
         bmi_status = BMI_SUCCESS
      case("gwres_sink")
         this%model%model_simulation%groundwater%gwres_sink= src
         bmi_status = BMI_SUCCESS
      case("gwres_stor")
         this%model%model_simulation%groundwater%gwres_stor = src
         bmi_status = BMI_SUCCESS
      case('gwsink_coef')
        this%model%model_simulation%groundwater%gwsink_coef = src
        bmi_status = BMI_SUCCESS
      case('gwflow_coef')
        this%model%model_simulation%groundwater%gwflow_coef = src
        bmi_status = BMI_SUCCESS

      case default
         bmi_status = BMI_FAILURE
      end select
    end function prms_set_float
    
    ! Set new double values.
    function prms_set_double(this, name, src) result (bmi_status)
      class (bmi_prms_groundwater), intent(inout) :: this
      character (len=*), intent(in) :: name
      double precision, intent(in) :: src(:)
      integer :: bmi_status
    
      select case(name)
      case("pkwater_equiv")
         this%model%model_simulation%climate%pkwater_equiv = src
         bmi_status = BMI_SUCCESS
      case("dprst_seep_hru")
         this%model%model_simulation%runoff%dprst_seep_hru = src
         bmi_status = BMI_SUCCESS
      case("dprst_stor_hru")
         this%model%model_simulation%runoff%dprst_stor_hru = src
         bmi_status = BMI_SUCCESS
      case default
         bmi_status = BMI_FAILURE
      end select
    end function prms_set_double
    
    ! Set integer values at particular locations.
    function prms_set_at_indices_int(this, name, inds, src) &
         result (bmi_status)
      class (bmi_prms_groundwater), intent(inout) :: this
      character (len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      integer, intent(in) :: src(:)
      integer :: bmi_status
      type (c_ptr) dest
      integer, pointer :: dest_flattened(:)
      integer :: i
    
      select case(name)
      case default
         bmi_status = BMI_FAILURE
      end select
    end function prms_set_at_indices_int
    
    ! Set real values at particular locations.
    function prms_set_at_indices_float(this, name, inds, src) &
         result (bmi_status)
      class (bmi_prms_groundwater), intent(inout) :: this
      character (len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      real, intent(in) :: src(:)
      integer :: bmi_status
      type (c_ptr) dest
      real, pointer :: dest_flattened(:)
      integer :: i, n_elements, status, gridid
    
      status = this%get_var_grid(name, gridid)
      status = this%get_grid_size(gridid, n_elements)

      select case(name)          
          !Values below are input from surface and soil to groundwater
      case("hru_intcpstor")
        dest = c_loc(this%model%model_simulation%intcp%hru_intcpstor(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case("soil_moist_tot")
        dest = c_loc(this%model%model_simulation%soil%soil_moist_tot(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case("soil_to_gw")
        dest = c_loc(this%model%model_simulation%soil%soil_to_gw(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case("ssr_to_gw")
        dest = c_loc(this%model%model_simulation%soil%ssr_to_gw(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case("ssres_flow")
        dest = c_loc(this%model%model_simulation%soil%ssres_flow(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case("hru_impervstor")
        dest = c_loc(this%model%model_simulation%runoff%hru_impervstor(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case("sroff")
        dest = c_loc(this%model%model_simulation%runoff%sroff(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case("gwres_flow")
        dest = c_loc(this%model%model_simulation%groundwater%gwres_flow(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case("gwres_sink")
        dest = c_loc(this%model%model_simulation%groundwater%gwres_sink(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case("gwres_stor")
        dest = c_loc(this%model%model_simulation%groundwater%gwres_stor(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case('gwsink_coef')
        dest = c_loc(this%model%model_simulation%groundwater%gwsink_coef(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case('gwflow_coef')
        dest = c_loc(this%model%model_simulation%groundwater%gwflow_coef(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS

      case default
         bmi_status = BMI_FAILURE
      end select
    end function prms_set_at_indices_float
    
    ! Set double values at particular locations.
    function prms_set_at_indices_double(this, name, inds, src) &
         result (bmi_status)
      class (bmi_prms_groundwater), intent(inout) :: this
      character (len=*), intent(in) :: name
      integer, intent(in) :: inds(:)
      double precision, intent(in) :: src(:)
      integer :: bmi_status
      type (c_ptr) dest
      double precision, pointer :: dest_flattened(:)
      integer :: i, n_elements, status, gridid
    
      status = this%get_var_grid(name, gridid)
      status = this%get_grid_size(gridid, n_elements)

      select case(name)
      case("pkwater_equiv")
        dest = c_loc(this%model%model_simulation%climate%pkwater_equiv(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case("dprst_seep_hru")
        dest = c_loc(this%model%model_simulation%runoff%dprst_seep_hru(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case("dprst_stor_hru")
        dest = c_loc(this%model%model_simulation%runoff%dprst_stor_hru(1))
        call c_f_pointer(dest, dest_flattened, [n_elements])
        do i = 1, size(inds)
            dest_flattened(inds(i)) = src(i)
        end do
        bmi_status = BMI_SUCCESS
      case default
         bmi_status = BMI_FAILURE
      end select
    end function prms_set_at_indices_double
    
    !! A non-BMI procedure for model introspection.
    !subroutine print_model_info(this)
    !  class (bmi_prms_groundwater), intent(in) :: this
    !
    !  call print_info(this%model)
    !end subroutine print_model_info

    end module bmiprmsgroundwater
