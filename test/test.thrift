namespace erlang scp

typedef string ExternalID
typedef string InternalID
typedef i64    IntegerInternalID

struct GeneratedID {
    1: required InternalID id
    2: optional IntegerInternalID integer_id
}

union GenerationSchema {
    1: SnowflakeSchema snowflake
    3: SequenceSchema sequence
}

struct SnowflakeSchema {}

struct SequenceSchema {
    1: required string sequence_id
    2: optional i64 minimum
}

service Generator {
    GeneratedID GenerateID (1: GenerationSchema schema)
}
