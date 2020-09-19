# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here
  All_Pieces = All_Pieces +
    [rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2,1]]),
    [[[0, 0], [-2, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
    [[0, 0], [0, -1], [0,-2], [0, 1], [0, 2]]],
    rotations([[0, 0], [1, 0], [0, 1]])
    ]

  def self.next_piece (board)
    Piece.new(All_Pieces.sample, board)
  end

  def self.cheat_piece(board)
    Piece.new([[0,0]], board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @start_cheat = false
  end

  def flip
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat
    if @start_cheat
      return
    end
    if @score >= 100
      @start_cheat = true
      @score -= 100
    end
  end

  def next_piece
    if @start_cheat
      @current_block = MyPiece.cheat_piece(self)
      @start_cheat = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.length()-1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here
  def initialize
    @root = TetrisRoot.new
    @timer = TetrisTimer.new
    set_board
    @running = true
    key_bindings
    key_bindings_1
    buttons
    run_game
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings_1
    @root.bind('u', proc{@board.flip})
    @root.bind('c', proc{@board.cheat})
  end
end


