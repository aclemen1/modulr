# File: ./modules/my/great/module/reflection.R

library(modulr)

"my/great/module/reflection#0.1.0" %provides% {
  list(
    .__name__ = .__name__,
    .__namespace__ = .__namespace__,
    .__initials__ = .__initials__,
    .__final__ = .__final__,
    .__version__ = .__version__,
    .__file__ = .__file__,
    .__path__ = .__path__
  )
}
