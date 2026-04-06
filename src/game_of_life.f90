module game_of_life
   use kinds_mod
   use grid_mod
   use terminal_mod

   implicit none
   private
   public :: initialize_game_state, calculate_next_generation, render, reset_game

   integer(rk), allocatable :: initial_grid(:, :)
   integer(rk), allocatable :: current_grid(:, :)
   integer(rk), allocatable :: next_grid(:, :)
   integer(rk) :: generation

contains

   subroutine initialize_game_state(file_grid, cols, rows)
      integer(rk), intent(in) :: file_grid(:, :)
      integer(rk), intent(in) :: cols, rows

      if (allocated(current_grid)) deallocate(current_grid)
      if (allocated(next_grid)) deallocate(next_grid)
      if (allocated(initial_grid)) deallocate(initial_grid)
      generation = 0

      allocate(current_grid(cols, rows))
      allocate(next_grid(cols, rows))
      allocate(initial_grid(cols, rows))
      initial_grid = file_grid(1:cols, 1:rows)
      current_grid = initial_grid
      next_grid = initial_grid
   end subroutine initialize_game_state

   subroutine calculate_next_generation()
      integer(rk) :: row, col, nrow, ncol
      integer(rk) :: living_neighbors
      integer(rk) :: row_count, col_count

      if (.not. allocated(current_grid)) then
         print '(A)', 'Game state has not been initialized.'
         stop 1
      end if

      row_count = size(current_grid, 1)
      col_count = size(current_grid, 2)

      do row = 1, row_count
         do col = 1, col_count
            living_neighbors = 0
            do nrow = max(1, row - 1), min(row_count, row + 1)
               do ncol = max(1, col - 1), min(col_count, col + 1)
                  if (nrow == row .and. ncol == col) cycle
                  living_neighbors = living_neighbors + current_grid(nrow, ncol)
               end do
            end do

            if (living_neighbors == 3) then
               next_grid(row, col) = 1
            else if (living_neighbors == 2) then
               next_grid(row, col) = current_grid(row, col)
            else
               next_grid(row, col) = 0
            end if
         end do
      end do

      current_grid = next_grid
      generation = generation + 1
   end subroutine calculate_next_generation

   subroutine render()
      if (.not. allocated(current_grid)) then
         print '(A)', 'Game state has not been initialized.'
         stop 1
      end if

      call clear_screen()
      print '(A,I3)', 'Generation: ', generation
      call print_grid(current_grid, size(current_grid, 1), size(current_grid, 2))
   end subroutine render

   subroutine reset_game()
      if (.not. allocated(initial_grid)) then
         print '(A)', 'Game state has not been initialized.'
         stop 1
      end if

      current_grid = initial_grid
      next_grid = initial_grid
      generation = 0
   end subroutine reset_game
end module
