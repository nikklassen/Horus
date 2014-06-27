OBJDIR = obj
BINDIR = bin
LIBDIR = src
APIDIR = textToMath-web
TESTDIR = tests

GHC_OPTIONS = -hidir obj -odir obj -O -j2
INCLUDES = -i$(LIBDIR) -i$(APIDIR)

server: textToMath-web/Server.hs | $(BINDIR)
		@echo "Building server"
		ghc $< $(INCLUDES) $(GHC_OPTIONS) -o $(BINDIR)/server

test_server: tests/TestServer.hs | $(BINDIR)
		@echo "Building test server"
		ghc $< $(INCLUDES) $(GHC_OPTIONS) -o $(BINDIR)/test_server

.PHONY: tests
tests: $(TESTDIR)/integration/TestSuite.hs test_server | $(BINDIR)
		@echo "Building tests"
		ghc $< $(INCLUDES) -i$(TESTDIR)/integration $(GHC_OPTIONS) -o $(BINDIR)/tests
		$(BINDIR)/test_server > /dev/null 2>&1 &
		-$(BINDIR)/tests
		-mocha tests/api
		@killall test_server

$(OBJDIR) $(BINDIR):
		@mkdir -p $@

clean:
		@echo "Cleaning..."
		@sudo rm -rf state $(OBJDIR)
