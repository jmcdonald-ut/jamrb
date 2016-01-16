require 'fileutils'

JamRb = {
  spec: File.dirname(__FILE__),
  root: File.expand_path('..', File.dirname(__FILE__))
}

RSpec.configure do |config|
  # Expectations configuration.
  config.expect_with :rspec do |expectations|
    expectations.include_chain_clauses_in_custom_matcher_descriptions = true
  end

  # Mock configuration.
  config.mock_with :rspec do |mocks|
    # Prevent mocking or stubbing method that does not exist.
    mocks.verify_partial_doubles = true
  end
end
