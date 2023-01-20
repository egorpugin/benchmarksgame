///bin/true;exec sw2 exec $0 -- "$@"

#include <bits/stdc++.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

using namespace std;
using namespace std::literals;
namespace fs = std::filesystem;
using namespace fs;

string open(auto &&fn) {
    auto fd = ::open(fn.string().c_str(), O_RDONLY);
    if (fd == -1) {
        //throw std::runtime_error{"cannot open file: " + fn.string()};
        return {};
    }
    auto sz = !exists(fn) ? 0 : file_size(fn);
    auto p = (char *)mmap(0, sz, PROT_READ, MAP_SHARED, fd, 0);
    if (p == MAP_FAILED) {
        ::close(fd);
        //throw std::runtime_error{"cannot create file mapping"};
        return {};
    }
    string s{p,sz};
    munmap(p,sz);
    close(fd);
    return s;
}

#define EXPECT(x) { if (!(x)) { cerr << __LINE__ << ": bad cond " #x "\n"; return 1; } }

int main() {
    current_path("public/program");
    for (path p : directory_iterator(".")) {
        if (!is_regular_file(p) || p.extension() != ".html") {
            continue;
        }

        auto s = p.stem().string();
        auto pos = s.find('-');
        auto pos2 = s.find('-', pos + 1);
        EXPECT(pos != -1 && pos2 != -1);
        auto dir = s.substr(0, pos);
        auto type = s.substr(pos+1, pos2-(pos+1));
        auto rest = s.substr(pos2 + 1);
        static map<string,string> exsts{
            {"chapel",".chpl"},

            {"clang",".c"},

            {"csharpcore",".cs"},
            {"csharppgo",".cs"}, // ?
            {"fsharpcore",".fs"}, // ?

            {"dartexe",".dart"}, // ?
            {"dartjit",".dart"}, // ?

            {"erlang",".erl"}, // ?
            {"hipe",".hipe"}, // ? .erl?

            {"gnat",".ada"}, // ?

            {"fpascal",".pas"},

            {"gcc",".c"},
            {"gpp",".cpp"},

            {"gfortran",".f"}, // ?
            {"ifc",".f"}, // ?

            {"ghc",".hs"}, // ?
            {"go",".go"},

            {"icc",".c"},
            {"icx",".cpp"},

            {"java",".java"},
            {"julia",".jl"}, // ?
            {"lua",".lua"},
            {"node",".js"},
            {"ocaml",".ml"}, // ?
            {"pharo",".st"}, // ? .mcz
            {"perl",".pl"},
            {"racket",".rkt"}, // ?
            {"ruby",".rb"},
            {"rust",".rs"},
            {"php",".php"},
            {"python3",".py"},
            {"swift",".swift"},
            {"typescript",".ts"},

            // unk
            {"sbcl",".sbcl"}, // ? list .core?
            {"vw",".vw"}, // ? smalltalk .im?
            {"mri",".mri"}, // ruby? .rb?
        };
        auto ext = exsts[type];
        if (ext.empty()) {
            cerr << "unk type " << type << "\n";
            continue;
        }
        auto outfn = path{dir} / "solution" / type / rest += ext;
        if (exists(outfn)) {
            continue;
        }

        cerr << "processing " << p << "\n";
        auto out = path{p} += ".txt";
        s = "lynx --dump \"" + p.filename().string() + "\" > \"" + out.filename().string() + "\"";
        if (!exists(out)) {
            system(s.c_str());
        }
        s = open(out);
        auto sc = "source code"sv;
        pos = s.find(sc);
        EXPECT(pos != -1);
        s = s.substr(pos + sc.size());
        while (!s.empty() && isspace(s[0])) {
            s = s.substr(1);
        }
        auto notes = "notes, command-line, and program output"sv;
        pos = s.find(notes);
        EXPECT(pos != -1);
        s = s.substr(0, pos);

        create_directories(outfn.parent_path());
        ofstream{outfn} << s;
    }
}
