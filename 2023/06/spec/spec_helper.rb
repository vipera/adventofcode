require 'rspec'
require 'rspec-parameterized'
require 'awesome_print'

RSpec.configure do |config|
  config.expect_with :rspec do |expectations|
    expectations.include_chain_clauses_in_custom_matcher_descriptions = true
  end
end
