#!/usr/bin/env python3
import re
import subprocess
import sys
import time

ENGINE = sys.argv[1] if len(sys.argv) > 1 else "./Amira"

BESTMOVE_RE = re.compile(r"^bestmove\s+([a-h][1-8][a-h][1-8][qrbn]?|0000)(\s+ponder\s+[a-h][1-8][a-h][1-8][qrbn]?)?$")


def read_until(proc, predicate, timeout=5.0):
    end = time.time() + timeout
    lines = []
    while time.time() < end:
        line = proc.stdout.readline()
        if not line:
            break
        line = line.strip()
        if line:
            lines.append(line)
            if predicate(line):
                return lines
    raise RuntimeError(f"Timed out waiting for expected output. Collected: {lines}")


def send(proc, cmd):
    proc.stdin.write(cmd + "\n")
    proc.stdin.flush()


def main():
    proc = subprocess.Popen(
        [ENGINE],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        bufsize=1,
    )

    try:
        send(proc, "uci")
        uci_lines = read_until(proc, lambda l: l == "uciok", timeout=5.0)
        if "uciok" not in uci_lines:
            raise RuntimeError("Did not receive uciok")

        send(proc, "isready")
        ready_lines = read_until(proc, lambda l: l == "readyok", timeout=5.0)
        if "readyok" not in ready_lines:
            raise RuntimeError("Did not receive readyok")

        send(proc, "position startpos moves e2e4 e7e5 g1f3")
        send(proc, "go depth 2")
        best_lines = read_until(proc, lambda l: l.startswith("bestmove "), timeout=10.0)
        bestmove_line = next((l for l in best_lines if l.startswith("bestmove ")), None)
        if bestmove_line is None or not BESTMOVE_RE.match(bestmove_line):
            raise RuntimeError(f"Invalid bestmove line: {bestmove_line}")

        send(proc, "quit")
        return 0
    finally:
        try:
            proc.kill()
        except Exception:
            pass


if __name__ == "__main__":
    raise SystemExit(main())
