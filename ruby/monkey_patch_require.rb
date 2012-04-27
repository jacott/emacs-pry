class <<$_emacs_monkey_patch
  def check(path)
    if @basename == File.basename(path,".rb")
      path="#{path}#{@extname}" if File.extname(path).empty?
      if $:.find { |dir| File.expand_path(path,dir) == self }
        ::Kernel.class_exec {
          alias require emacs_monkey_patch_original_require
          undef emacs_monkey_patch_original_require
        }

        eval($_emacs_monkey_patch_source,@main,self)
        alias require_relative emacs_monkey_patch_original_require_relative
        undef emacs_monkey_patch_original_require_relative
      end
    end
  end

  def _init(main)
    @main=main
    @basename=File.basename(self,".rb")
    @extname=File.extname(self)
    $" << self
  end
  private :_init
end
$_emacs_monkey_patch.send(:_init,binding)

module ::Kernel
  alias emacs_monkey_patch_original_require require
  alias emacs_monkey_patch_original_require_relative require_relative

  def require path
    emacs_monkey_patch_original_require(path) || $_emacs_monkey_patch.check(path)
  end

  def require_relative path
    require File.expand_path(path,File.dirname(caller.first.split(':').first))
  end

  private :require, :emacs_monkey_patch_original_require
end
