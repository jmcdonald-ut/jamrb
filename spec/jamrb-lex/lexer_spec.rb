RSpec.describe 'jamrb-lex', :output_specs do
  project_path = JamRb[:root] + "/jamrb-lex"
  fixtures_path = JamRb[:root] + "/jamrb-lex/test/rb-sx-tests"

  make_executable_in project_path

  fetch_fixtures(fixtures_path).each do |fixture|
    input = File.basename fixture
    output = "#{input}.out"

    navigate_to project_path
    result, status = execute "./bin/jamrb_lex < #{fixture}"

    describe 'lexer' do
      it 'exits with zero status' do
        expect(status).to eq 0
      end

      it "tokenizes #{input} to match the contents of #{output}" do
        expect(result).to eq File.read "#{fixture}.out"
      end
    end
  end
end
