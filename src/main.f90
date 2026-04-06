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

   integer(rk), parameter :: rows = 21, cols = 21
   integer(rk) :: grid(rows, cols)
   integer(rk) :: alternate(rows, cols)
   integer(rk), allocatable :: neighbors(:, :)

   integer(rk) :: living_neighbors = 0
   integer(rk) :: row, col
   integer(rk) :: start_row, end_row, start_col, end_col
   integer(rk) :: neighbor_rows, neighbor_cols
   integer(rk) :: generation, generations = 10

   integer(rk) :: arg_size
   character(len=32) :: arg

   arg_size = command_argument_count()
   if (arg_size >= 1) then
      call get_command_argument(1, arg)
      read(arg, *) generations
   end if

   grid = transpose(reshape([&
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 &
      ], [cols, rows]))
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
      call usleep(200000)
      call clear_screen()
      call print_grid(alternate, rows, cols)
      grid = alternate
   end do
end program game_of_life
