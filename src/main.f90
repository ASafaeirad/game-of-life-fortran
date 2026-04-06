subroutine print_grid(g, row, col)
   use kinds_mod
   implicit none

   integer(rk), intent(in) :: row, col
   integer(rk), intent(in) :: g(row, col)
   integer(rk) :: r, c

   do r = 1, row
      do c = 1, col
         if (g(r, c) == 1) then
            write(*, '(A)', advance='no') '██'
         else
            write(*, '(A)', advance='no') '░░'
         end if
      end do
      print *, ""
   end do
end subroutine print_grid

subroutine clear_screen()
   implicit none
   character(len=*), parameter :: clear = char(27)//"[2J"//char(27)//"[H"

   print *, clear
end subroutine clear_screen

program game_of_life
   use kinds_mod
   implicit none

   interface usleep
      subroutine usleep(microseconds) bind(C)
         use iso_c_binding, only: c_int
         integer(c_int), value :: microseconds
      end subroutine usleep
   end interface usleep

   integer(rk) :: rows = 1, cols = 1
   integer(rk) :: file_grid(100, 100)
   integer(rk), allocatable :: grid(:, :)
   integer(rk), allocatable :: alternate(:, :)
   integer(rk), allocatable :: neighbors(:, :)

   integer(rk) :: living_neighbors = 0
   integer(rk) :: row, col
   integer(rk) :: start_row, end_row, start_col, end_col
   integer(rk) :: neighbor_rows, neighbor_cols
   integer(rk) :: generation, generations = 10
   integer(rk) :: c
   character(len=2) :: cell

   integer(rk) :: arg_count, arg_index
   real:: arg_speed
   integer(rk) :: speed = 100000
   character(len=256) :: arg
   character(len=256) :: input_path = "input"
   integer(rk) :: unit = 10, ios
   character(len=256) :: line

   file_grid = 0

   arg_count = command_argument_count()
   arg_index = 1
   do while (arg_index <= arg_count)
      call get_command_argument(arg_index, arg)

      select case (trim(arg))
       case ('-g')
         if (arg_index >= arg_count) then
            print '(A)', 'Missing value for -g'
            stop 1
         end if

         arg_index = arg_index + 1
         call get_command_argument(arg_index, arg)
         read(arg, *, iostat=ios) generations
         if (ios /= 0) then
            print '(A)', 'Invalid value for -g'
            stop 1
         end if
       case ('-f')
         if (arg_index >= arg_count) then
            print '(A)', 'Missing value for -f'
            stop 1
         end if

         arg_index = arg_index + 1
         call get_command_argument(arg_index, input_path)
       case ('-s')
         if (arg_index >= arg_count) then
            print '(A)', 'Missing value for -s'
            stop 1
         end if
         arg_index = arg_index + 1
         call get_command_argument(arg_index, arg)
         read(arg, *, iostat=ios) arg_speed
         if (ios /= 0) then
            print '(A)', 'Invalid value for -s'
            stop 1
         end if
         if (arg_speed <= 0.0) then
            print '(A)', 'Value for -s must be greater than 0'
            stop 1
         end if
         speed = int(real(speed, kind=kind(arg_speed)) / arg_speed, kind=rk)

       case default
         print '(2A)', 'Unknown argument: ', trim(arg)
         stop 1
      end select

      arg_index = arg_index + 1
   end do

   open(unit, file=trim(input_path), status='old', action='read', iostat=ios)
   if (ios /= 0) then
      print '(2A)', 'Unable to open input file: ', trim(input_path)
      stop 1
   end if

   do
      read(unit, '(A)', iostat=ios) line
      if (ios /= 0) then
         rows = rows - 1
         exit
      end if
      cols = max(cols, len_trim(line))

      do c = 1, len_trim(line)
         cell = line(c:c)
         if (cell == '.') then
            file_grid(rows, c) = 0
         else if (cell == '#') then
            file_grid(rows, c) = 1
         end if
      end do

      rows = rows + 1
   end do
   close(unit)

   allocate(grid(rows, cols))
   allocate(alternate(rows, cols))
   grid = file_grid(1:rows, 1:cols)
   alternate = grid

   call clear_screen()
   call print_grid(grid, rows, cols)

   do generation = 1, generations
      do row = 1, rows
         do col = 1, cols
            living_neighbors = 0
            start_row = max(1, row-1)
            end_row = min(rows, row+1)
            start_col = max(1, col-1)
            end_col = min(cols, col+1)
            neighbor_rows = end_row - start_row + 1
            neighbor_cols = end_col - start_col + 1

            allocate(neighbors(neighbor_rows, neighbor_cols))
            neighbors = grid(start_row:end_row, start_col:end_col)

            living_neighbors = sum(neighbors) - grid(row, col)
            deallocate(neighbors)

            if (living_neighbors == 3) then
               alternate(row, col) = 1
            else if (living_neighbors == 2) then
               alternate(row, col) = grid(row, col)
            else
               alternate(row, col) = 0
            end if
         end do
      end do
      call usleep(speed)
      call clear_screen()
      print '(A,I3)', "Generation: ", generation
      call print_grid(alternate, rows, cols)
      grid = alternate
   end do
end program game_of_life
