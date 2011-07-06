#!/usr/bin/env ruby

class VM

  attr_accessor :arrays, :regs, :pc

  def initialize(program_file)
    @arrays = []
    @regs = [0, 0, 0, 0, 0, 0, 0]
    @pc = 0
    load_program(program_file)
  end

  def load_program(program_file)
    @arrays[0] = IO.read(program_file).unpack("N*")
  end

  def check_oob(array_reg, index_reg)
    array = @regs[array_reg]
    index = @regs[index_reg]
    if array >= @arrays.size
      $stderr.puts "illegal array number #{array} (there are only #{@arrays.size} arrays)"
      return
    end
    if @arrays[array].nil?
      $stderr.puts "array[#{array}]is nil"
      return
    end
    if index >= @arrays[array].size
      $stderr.puts "illegal array[#{array}] index #{index} (size of array[#{array}] is #{@arrays[array].size})"
    end
  end

  def run
    while true
      $stderr.puts to_debug_s() if $DEBUG
      ins = @arrays[0][@pc]
      @pc += 1
      a, b, c = ((ins >> 6) & 7), ((ins >> 3) & 7), (ins & 7)
      case (ins >> 28)
      when 0                    # move
        @regs[a] = @regs[b] if @regs[c] != 0
      when 1                    # array index
        check_oob(b, c) if $DEBUG
        @regs[a] = @arrays[@regs[b]][@regs[c]]
      when 2                    # array amendment
        check_oob(a, b) if $DEBUG
        @arrays[@regs[a]][@regs[b]] = @regs[c]
      when 3                    # addition
        @regs[a] = (@regs[b] + @regs[c]) & 0xffffffff
      when 4                    # multiplication
        @regs[a] = (@regs[b] * @regs[c]) & 0xffffffff
      when 5                    # division
        @regs[a] = (@regs[b] / @regs[c]) & 0xffffffff
      when 6                    # not-and
        @regs[a] = ~(@regs[b] & @regs[c]) & 0xffffffff
      when 7                    # halt
        exit 0
      when 8                    # allocation
        # Weird: if I use @regs[c] in the Array.new call below, some values
        # (those above 32768?) get changed to a lower value like 2. Reading
        # reg C here fixes that problem.
        size = @regs[c]
        @regs[b] = @arrays.index(nil) || @arrays.size
        @arrays[@regs[b]] = Array.new(size, 0)
      when 9                    # abandonment
        @arrays[@regs[c]] = nil
      when 10                   # output
        print @regs[c].chr
        # $stdout.flush
      when 11                   # input
        @regs[c] = getch
      when 12                   # load program
        if @regs[b] > 0
          @arrays[0] = @arrays[@regs[b]].dup
        end
        @pc = @regs[c]
      when 13                   # load immediate
        @regs[(ins >> 25) & 7] = ins & 0x1ffffff
        $stderr.puts "\tloadi r[#{(ins >> 25) & 7}] = #{ins & 0x1ffffff}" if $DEBUG
      else
        $stderr.puts "illegal instruction"
        exit 1
      end
    end
  end

  def to_debug_s(decr_pc=false)
    @pc -= 1 if decr_pc
    ins = @arrays[0][@pc]
    a, b, c = ((ins >> 6) & 7), ((ins >> 3) & 7), (ins & 7)
    opcode = (ins >> 28)
    str = "pc = #{'%5d' % pc}"
    str << ", ins = 0x#{'%08x' % ins} 0#{'%011o' % ins}"
    str << ", op = #{'%2d' % opcode}"
    str << ", a = #{a}, b = #{b}, c = #{c}"
    str << ", ra = #{@regs[a]}, rb = #{@regs[b]}, rc = #{@regs[c]}"
    str << " regs = #{regs}"
    str << ' "'
    @regs.each { |val| str << (val.nil? ? "*" : (val >= 32 && val <= 126 ? val.chr : '.')) }
    str << '"'
    @pc += 1 if decr_pc
    str
  end

end

if __FILE__ == $0 && ARGV[0]
  VM.new(ARGV[0]).run
end
