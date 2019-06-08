from setuptools import setup, Extension
from Cython.Build import cythonize
import numpy as np

setup(
    name='_segment',
    ext_modules=cythonize(
        Extension(
            "_segment",
            sources=["_segment.pyx"],
            include_dirs=[np.get_include()]
        )
    ),
    install_requires=["numpy"]
)