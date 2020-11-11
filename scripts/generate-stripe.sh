export PATH="$HOME/.local/bin:$PATH"

generatorRepo=${1:-stripe-code-generator}
outputDir=${2:-stripe-api}
openapi3-code-generator-exe "$generatorRepo/.circleci/specifications/stripe-api.yml" \
  --property-type-suffix="'" \
  --module-name "StripeAPI" \
  --convert-to-camel-case \
  --package-name "stripe-api" \
  --output-dir "$outputDir" \
  --force \
  \
  --omit-additional-operation-functions \
  --operation-to-generate "GetEvents" \
  --operation-to-generate "GetCustomers" \
  --operation-to-generate "PostCheckoutSessions" \
  --operation-to-generate "GetPlansPlan" \
  --white-listed-schema event \
  --white-listed-schema notification_event_data \
  --white-listed-schema checkout.session \
  --white-listed-schema customer \
  --white-listed-schema plan

tree $outputDir/src/StripeAPI/
