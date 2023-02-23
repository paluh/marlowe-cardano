-- Deploy chain:fix-upper-validity to pg
-- requires: split-address

BEGIN;

-- Fix for `mainnet`.
UPDATE chain.tx
  SET validityupperbound = 9223372036854775807
  WHERE id in (
    '\x0b73eec399e96b45c82663c4cae68d3cf1dc04471ea78d7b02bf20571a821f7b'
  , '\x4941f58b1b6c6ab3660a0446445f72435c3fea59109f79a64efb919e4556cf77'
  , '\x0395c035934fae23337bb18def38674debb7343fbbda91fa416bfb72a480adbe'
  , '\x2fc184d585bb468312878f37ec53d0409527995f1edd227c2040009be19e360e'
  , '\x3cf009615d42deaa3cd1ec8b962a5fd4bee7e98211a09b5b9bbc4190157cb844'
  , '\x55908485f00d7156fcd1f7e77e49b28405d6083be345f9be43c5b1755b0f5d6f'
  , '\x5738f941a3a422345d210b76056695dad68d5dd2653fb226fd8e2d36b64d1618'
  , '\x5b4900f98ed5da0e35c68ec00db551b9e50e41543922966375d1e7578c2a0564'
  , '\x65e261e8026a439053c35531357f8dc49b713942245c4adf080964c04634e813'
  , '\x6b397a25c9a70a2d298129a64ebfe358c83bf8743d10d9ba196e5a77ffdbc24b'
  , '\x85fcc185fd369cd1058f4367cb5506fdae7de74115144fba87fdc7b8275785af'
  , '\x874aa6a6826763e6721e417e2e4333c082fb732ffdfe764a9fddec0f108cdb11'
  , '\x8fbf260946928ff56f2639a903757686084a7e1ed648f21f7098d88001a520c8'
  , '\x9c27b84febdc2ad89f9c9bda2229094eabe8b507ad006736639a73b363c676f2'
  , '\xa4a2bbb3791f66657e8a2b505678fc19635e3802dc3da6fa6f34f2e0d1ee6904'
  , '\xac9bb7a310438df905ab3da84579d23d3e1d36e641537d984174a095544ade47'
  , '\xb54f46fded05a5f08967a5eda73b78b2d2f8ee5436753efaf64cb2d9f1aedd77'
  , '\xc74bcbdff9429618d8b6d7aba53ad15017b9ed8f01ca2ea74a896bb43908ec50'
  , '\xcf6ec516acbb1eaf242748c5d6c3b63657d160ef8593b85b7b8d10bbe0ff876d'
  , '\xda4a49e31ff464a97ce012ce714d5c64730d73c4562cd536ad94624212c9d8c8'
  , '\xde9a1efec167ed0547148d869f3cf8010f4190bbbc5945186483961e80274438'
  , '\xe031f58c9d5f56bbf47abe6bbe0e2b5cbd79b88f8a13fbd024e6935169033068'
  , '\xe5d29a97edbbc2eef4ae249ec43f2fadd5ec7cb157645d75f1d4e3bdb43ad48c'
  );

COMMIT;
