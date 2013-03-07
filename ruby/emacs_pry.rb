begin
  require 'termios'
rescue LoadError
end
require 'pry'
require 'pry-nav' rescue nil

# # FIXME
# module Kernel
#   def fixme(*args)
#     STDOUT.puts args.inspect
#     STDOUT.flush
#   end
# end

def self._emacs_setup(main)
  class <<self
    undef _emacs_setup
  end

  ENV['TERM']='eterm-color'

  if defined? Termios
    class <<STDIN
      include Termios
    end

    attrs=STDIN.tcgetattr

    flag_map=[:iflag, :oflag, :cflag, :lflag].inject({}) do |h,sym|
      for name in Termios::const_get "#{sym.to_s.upcase}_NAMES"
        h[name]=sym
      end
      h
    end

    proc = ->(sym) do
      off=sym[0]=="-"
      sym=sym[1..-1] if off
      sym=sym.upcase.to_sym
      imode=flag_map[sym]
      if imode
        bit=::Termios.const_get(sym)
        cval=attrs.send(imode)
        if off
          cval &= ~bit
        else
          cval |= bit
        end
        attrs.send("#{imode}=",cval)
      end
    end

    # raw
    # %w[-ignbrk -brkint -ignpar -parmrk -inpck -istrip -inlcr -igncr -icrnl  -ixon  -ixoff  -iuclc  -ixany -imaxbel -opost -isig -icanon -xcase].each do |sym|
    #   proc.call(sym)
    # end

    # STDIN.tcsetattr(::Termios::TCSANOW, attrs)


    # Pry.config.should_load_rc = false

    # require "awesome_print"

    # Pry.print = proc { |output, value| output.puts value.ai }


    # sane
    %w[cread -ignbrk brkint -inlcr -igncr icrnl -iutf8 -ixoff -iuclc -ixany imaxbel opost -olcuc -ocrnl onlcr
    -onocr -onlret -ofill -ofdel nl0 cr0 tab0 bs0 vt0 ff0 isig icanon iexten  echo  echoe  echok  -echonl  -noflsh
    -xcase -tostop -echoprt echoctl echoke].each do |sym|
      proc.call(sym)
    end

    STDIN.tcsetattr(::Termios::TCSANOW, attrs)
  else
    system('stty sane')
  end

  Pry.config.color = true

  $_emacs_pid, $_emacs_monkey_patch=(ENV['_EMACS_MONKEY_PATCH']||'').split(':')
  if $_emacs_pid
    fn="/tmp/emacs_pry_#{$_emacs_pid}.rb"
    $_emacs_monkey_patch=File.expand_path($_emacs_monkey_patch)

    File.open(fn,"r") {|io| $_emacs_monkey_patch_source=io.read}
    File.unlink(fn)

    $0= ARGV.shift

    require_relative 'monkey_patch_require'
    if File.expand_path($0) == File.expand_path($_emacs_monkey_patch)
      $0=$_emacs_monkey_patch_source
      eval($_emacs_monkey_patch_source,main,$_emacs_monkey_patch)
    else
      load(File.expand_path($0))
      ::Kernel.class_exec {
        alias require emacs_monkey_patch_original_require
        undef emacs_monkey_patch_original_require
        alias require_relative emacs_monkey_patch_original_require_relative
        undef emacs_monkey_patch_original_require_relative
      }
    end
  end
end

self._emacs_setup(binding)
