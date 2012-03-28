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

  def require path
    emacs_monkey_patch_original_require(path) || $_emacs_monkey_patch.check(path)
  end

  private :require, :emacs_monkey_patch_original_require
end
