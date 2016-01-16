RSpec.describe 'jamrb-lex' do
  # Before the specs are run re-make the lexer binary.
  before :all do
    FileUtils.cd JamRb[:root]
    FileUtils.cd JamRb[:root] + "/jamrb-lex"

    system "make clean > /dev/null"
    fail IOError, "`$ make clean` failed!" if $? != 0

    system "make > /dev/null"
    fail IOError, "`$ make` failed!" if $? != 0
  end

  # Ensure that we are in the lexer directory.
  before(:each) { FileUtils.cd JamRb[:root] + "/jamrb-lex" }

  context 'input file' do
    path = JamRb[:root] + "/jamrb-lex/test/rb-sx-tests"
    entries = Dir.entries path

    valid_files = entries.keep_if { |f| File.file? path + "/#{f}" }
    file_paths = valid_files.map { |f| path + "/#{f}" }
    in_files = file_paths.keep_if { |f| f.match(/.*\.out/).nil? }

    in_files.each do |in_file|
      next unless File.exists? in_file + ".out"

      in_name = File.basename(in_file)
      out_name = "#{in_name}.out"

      it "tokenizes contents of #{in_name} to match contents of #{out_name}" do
        expected = File.read(in_file + ".out")
        result = `./bin/jamrb_lex < #{in_file}`
        expect(result).to eq expected
      end
    end
  end
end
