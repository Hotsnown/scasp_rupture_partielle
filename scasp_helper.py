"""Utilities for running s(CASP) programs from Python.

This module provides a simple wrapper around the `scasp` command-line
utility.  It expects the `scasp` executable to be installed and available on
the system `PATH`.
"""

from __future__ import annotations

import subprocess
from pathlib import Path
from typing import List, Optional


def run_scasp(program: str, query: Optional[str] = None) -> str:
    """Run the s(CASP) solver on a Prolog program.

    Parameters
    ----------
    program:
        Path to a `.pl` file containing the s(CASP) program.
    query:
        Optional query to pass to the solver.  If provided, it is appended to
        the command line after the program path.

    Returns
    -------
    str
        Standard output produced by the solver.
    """
    cmd: List[str] = ["scasp", program]
    if query:
        cmd.append(query)

    completed = subprocess.run(
        cmd,
        check=True,
        capture_output=True,
        text=True,
    )
    return completed.stdout


if __name__ == "__main__":
    program_path = Path("rupture_partielle.pl")
    try:
        print(run_scasp(str(program_path)))
    except FileNotFoundError:
        print(
            "s(CASP) command not found. Please install the `scasp` package and ensure the `scasp` executable is in your PATH."
        )
