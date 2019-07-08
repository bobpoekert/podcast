from setuptools import setup, Extension
from Cython.Build import cythonize
import numpy as np

setup(
    name='_segment',
    ext_modules=cythonize(
        Extension(
            "_ccomp",
            sources=["_ccomp.pyx"],
            include_dirs=[np.get_include()]
        )
    ) + cythonize(
        Extension(
            "_segment",
            sources=["_segment.pyx"],
            include_dirs=[np.get_include()]
        )
    ) + cythonize(
        Extension(
            "quickshift",
            sources=["quickshift.pyx"],
            include_dirs=[np.get_include()],
            #extra_compile_args=['-fopenmp'],
            #extra_link_args=['-fopenmp'],
        )
    ),
    install_requires=["numpy"]
)