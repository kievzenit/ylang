// Golden tests: every tests/golden/*.y file is compiled and the result is
// compared with the matching *.golden file.
//
// The result is either the LLVM IR (if compilation succeeds) or the compiler's
// diagnostics (if it fails). A compiler panic is always a test failure.
//
// Regenerate golden files after an intended change with:
//
//	make test-update        (or: go test -tags llvm21 ./tests -update)
//
// and review the diff with git before committing.
package tests

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"runtime/debug"
	"strings"
	"testing"

	"github.com/kievzenit/ylang/internal/compiler_errors"
	"github.com/kievzenit/ylang/internal/driver"
)

var update = flag.Bool("update", false, "rewrite golden files with the current output")

const goldenDir = "golden"

// compileAborted is the panic value used to unwind out of the compiler when
// the error handler's FailNow is called.
type compileAborted struct{}

func TestGolden(t *testing.T) {
	sources, err := filepath.Glob(filepath.Join(goldenDir, "*.y"))
	if err != nil {
		t.Fatal(err)
	}
	if len(sources) == 0 {
		t.Fatalf("no test sources found in %s", goldenDir)
	}

	for _, source := range sources {
		name := strings.TrimSuffix(filepath.Base(source), ".y")
		t.Run(name, func(t *testing.T) {
			got := compile(t, source)
			goldenPath := strings.TrimSuffix(source, ".y") + ".golden"

			if *update {
				if err := os.WriteFile(goldenPath, []byte(got), 0o644); err != nil {
					t.Fatal(err)
				}
				return
			}

			want, err := os.ReadFile(goldenPath)
			if os.IsNotExist(err) {
				t.Fatalf("missing %s; run with -update to create it.\noutput:\n%s", goldenPath, got)
			}
			if err != nil {
				t.Fatal(err)
			}
			if got != string(want) {
				t.Errorf("output differs from %s (run with -update to accept)\n%s", goldenPath, diff(string(want), got))
			}
		})
	}
}

// compile runs the compiler on path and returns the IR, or the diagnostics if
// compilation failed. The path is passed through as given (relative), so
// diagnostics in golden files do not depend on where the repo is checked out.
func compile(t *testing.T, path string) (output string) {
	t.Helper()

	source, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	fileName := filepath.ToSlash(path)

	var diagnostics bytes.Buffer
	eh := compiler_errors.NewErrorHandlerWithExit(&diagnostics, func() { panic(compileAborted{}) })

	defer func() {
		r := recover()
		if r == nil {
			return
		}
		if _, ok := r.(compileAborted); ok {
			output = diagnostics.String()
			return
		}
		t.Fatalf("compiler panicked: %v\n%s", r, debug.Stack())
	}()

	fileHir := driver.Analyze(fileName, source, eh)
	module := driver.Emit(fileHir)
	defer module.Dispose()
	return module.String()
}

// diff returns a minimal line diff: the first differing line with context.
func diff(want, got string) string {
	wantLines := strings.Split(want, "\n")
	gotLines := strings.Split(got, "\n")
	for i := 0; i < len(wantLines) || i < len(gotLines); i++ {
		var w, g string
		if i < len(wantLines) {
			w = wantLines[i]
		}
		if i < len(gotLines) {
			g = gotLines[i]
		}
		if w != g {
			return fmt.Sprintf("first difference at line %d:\n  want: %q\n  got:  %q\n\nfull output:\n%s", i+1, w, g, got)
		}
	}
	return ""
}
