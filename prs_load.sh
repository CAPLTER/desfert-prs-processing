#!/usr/bin/env bash
#
# Load a WesternAg PRS Excel file into urbancndep.prs_analysis.
#
# The database password is not accepted as an argument; set PGPASSWORD or use
# ~/.pgpass.

set -euo pipefail

usage() {
  cat <<USAGE
Usage: $(basename "$0") -i FILE --user USER [options]

  -i, --input FILE     WesternAg PRS Excel file (required)
  -n, --dry-run        perform the insert, then roll back (nothing is written)
      --host HOST      database host (default: localhost)
      --dbname DB      database (default: caplter)
      --user USER      database user (required)
      --port PORT      database port (default: 5432)
  -h, --help           show this help

Exit status is non-zero if any validation or database step fails.
USAGE
}

args=()

while [[ $# -gt 0 ]]; do
  case "$1" in
    -i|--input)   [[ $# -ge 2 ]] || { echo "ERROR: $1 requires a value" >&2; exit 2; }
                  args+=(--input "$2"); shift 2 ;;
    -n|--dry-run) args+=(--dry-run); shift ;;
    --host|--dbname|--user|--port)
                  [[ $# -ge 2 ]] || { echo "ERROR: $1 requires a value" >&2; exit 2; }
                  args+=("$1" "$2"); shift 2 ;;
    -h|--help)    usage; exit 0 ;;
    *)            echo "ERROR: unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
done

if [[ ! " ${args[*]:-} " =~ " --input " ]]; then
  echo "ERROR: --input is required" >&2
  usage >&2
  exit 2
fi

if [[ ! " ${args[*]:-} " =~ " --user " ]]; then
  echo "ERROR: --user is required" >&2
  usage >&2
  exit 2
fi

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

exec Rscript "${script_dir}/prs_cli.R" "${args[@]}"
