/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","f41f4766cbb5cdc487c10225319e77fc"],["/about/index.html","64f7b857f6325dbb2e6e382da549237d"],["/archives/2023/01/index.html","120f39dc45a9c5d7f13bbb1822e424d6"],["/archives/2023/02/index.html","1ff1f980325be9eb6264a3e001b8a0aa"],["/archives/2023/02/page/2/index.html","91c33ae1efadf4b3bce23bdf49984240"],["/archives/2023/03/index.html","dedc257b8901f55fdc91ae4967550417"],["/archives/2023/05/index.html","107f8c748512831a8935462cc0e2ec76"],["/archives/2023/06/index.html","3dd97b1f2aa6f85992f5cb516d6ff167"],["/archives/2023/09/index.html","51d7e4147675f406923f70e6fafcdcb1"],["/archives/2023/11/index.html","2e2872bb8b33cf3109cb0bc4635b4de5"],["/archives/2023/12/index.html","f30009c0fc21933d08c1817938daef9d"],["/archives/2023/index.html","11429285ab6c1655e5de9fefcce7c316"],["/archives/2023/page/2/index.html","5d2b0ccc2ed66a280be15af85f9c4fa5"],["/archives/2023/page/3/index.html","54f6552313d47b11f219087039c2ed4e"],["/archives/2023/page/4/index.html","803d830c5d2eaf79049b207843e00896"],["/archives/2024/02/index.html","beb4de916cca64919c82c4961898d2c4"],["/archives/2024/index.html","5dc80b09f80647e4db31964c16a7bd7e"],["/archives/index.html","2def4f347368089a5738f1ac09b308b1"],["/archives/page/2/index.html","9c0e82306b25cf15e6b5b59f2ef80212"],["/archives/page/3/index.html","25199de3fe6b442ac7e18500c183c19a"],["/archives/page/4/index.html","0744d0821fd2df0ff7b201a0896a228f"],["/baidu_verify_codeva-qQP2iZOMLX.html","6748b76698b911624c7440f1abc7fb7b"],["/categories/Java/index.html","d309d9f0e04fa212225534655be99e5c"],["/categories/Java/后端/index.html","dc2febe48c3d89965a0cb088038eb119"],["/categories/Java/基础/index.html","c41af2af78b749c71b7a41c1aedb0d28"],["/categories/Java/基础/集合/index.html","9f34b370222963959f60aca3aac3cd71"],["/categories/Python/index.html","84cc0b2699cfd23a8073359c9509e06a"],["/categories/Python/编程环境/index.html","06caefeef02436a7386daed013eeac3a"],["/categories/R语言/index.html","0ff3adf0e22007597a340727f6e8fb27"],["/categories/R语言/编程环境/index.html","3ae8156262dcb7c80d30dd5a411f2668"],["/categories/index.html","50d8d938c695d5c6f5cbba80da86a5e0"],["/categories/中间件/index.html","4511570e8c2633598a2e6629faecb28c"],["/categories/前端/Vue/index.html","b05d530fba4efa663c98d589dca8d681"],["/categories/前端/index.html","2bed07ea7b705f790c1a03dba491be6d"],["/categories/大数据开发/ElasticSearch/index.html","33145125361a65958554cd9b60bcde00"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","4025b29ca8bcf086929c3eb79fba004e"],["/categories/大数据开发/HBase/index.html","ebc364ac45545ca16d9cf2902f9ddebc"],["/categories/大数据开发/HBase/学习笔记/index.html","c86089ffcef60f60030ed8ccc4e90aac"],["/categories/大数据开发/HBase/环境搭建/index.html","9d9477757ecde8a1553b6d1621a8579d"],["/categories/大数据开发/Hadoop/index.html","640bda0632f54f86a3f7a77fd6f7025e"],["/categories/大数据开发/Hadoop/技术/index.html","8bc6f03125093aa8f13bd4319e21080e"],["/categories/大数据开发/Hadoop/环境搭建/index.html","2a5d310ae832741df9d1d7648c6b4786"],["/categories/大数据开发/Redis/index.html","98cf04b70c2e376b60a6a54914c018b6"],["/categories/大数据开发/Redis/技术/index.html","0f925d1062ac2fa198d1be1ea1430460"],["/categories/大数据开发/Redis/环境搭建/index.html","c676d0e4fc1d64c8d26a6ff3ee6a87e8"],["/categories/大数据开发/Spark/index.html","0a26b7d4ab7823e3dea6399a0de5e8f1"],["/categories/大数据开发/Spark/环境搭建/index.html","39dbe873aa6bbb38aa0b5d16f49371f7"],["/categories/大数据开发/Zookeeper/index.html","130500056e7259bcc64a46247a0a23eb"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","a17ab817bd4fd5af1284da12ade3e9e5"],["/categories/大数据开发/index.html","c25d486b1f49e522ab4bb32318649fe1"],["/categories/学校课程/index.html","e1b5b4ca789a239f9712973e4719aad1"],["/categories/学校课程/计算机操作系统/index.html","f682b288e45657ede47b082163820fec"],["/categories/操作系统/Linux/index.html","6daec14dc12ba58b8ed1425616f72397"],["/categories/操作系统/Mac/index.html","6ce69b5d4a8fb3c2c2e0d25b89706127"],["/categories/操作系统/Windows/index.html","674cba605a81a7291e22670aeb49c73b"],["/categories/操作系统/index.html","5b3b8de7c8fe8cafd4ecd2adf2b2de1c"],["/categories/数学建模/index.html","d76815e58db3edc1d75c7d9f32fdddb4"],["/categories/数学建模/latex/index.html","aec1e823dd28b7130b381d7f9c6a4536"],["/categories/数学建模/优化类/index.html","5793aa3a16bcb35d53e102e7dd386f81"],["/categories/数学建模/优化类/现代优化算法/index.html","0026dc54b6eba08a1ff8fb45030fbccc"],["/categories/数学建模/优化类/规划类/index.html","ff672cc6f7e5138eae06a97451b83332"],["/categories/数学建模/绘图/index.html","38a7c3b6b1f6cde48fdb1241e46a367a"],["/categories/数据库/MySQL/index.html","0db5845b296effe750af8b873a817452"],["/categories/数据库/index.html","fd90b965aa17c571243b19aa6c1a4448"],["/categories/数据结构和算法/index.html","ed21dd9d340caf346fec9a70c101d93a"],["/categories/数据结构和算法/page/2/index.html","51a6dbadb0e2d060eacda55cc84c3040"],["/categories/数据结构和算法/基本原理/bfs/index.html","c1b9e6c55432e0c77aa00109f181c990"],["/categories/数据结构和算法/基本原理/dfs/index.html","bcd16888348b1f789f69f7505d3cf59a"],["/categories/数据结构和算法/基本原理/index.html","d7c5c4fb41edacb3afc9988439b6eb0c"],["/categories/数据结构和算法/基本原理/动态规划/index.html","0affe7315a605c73255b152bfb67c8b0"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","858adc291459d776e588cd4e6ccc9b45"],["/categories/数据结构和算法/基本原理/图论/index.html","4976b6459dfeff077deb0b104ee3f145"],["/categories/数据结构和算法/基本原理/字符串/index.html","c949e7a642e19166cef0ef6eb64f6e93"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","1bd1eb57ea55657ef43e6c6b03366b2b"],["/categories/数据结构和算法/基本原理/数论/index.html","102c8ef89640108a15396c0e72055a37"],["/categories/数据结构和算法/基本原理/树论/index.html","aad3ec5af108176e614b60c4877e8a69"],["/categories/数据结构和算法/基本原理/链表/index.html","4c32be51ef4f1e47c0235ab3be21a8ad"],["/categories/数据结构和算法/算法题/index.html","aa70a376242d2d5505f482e20ca526e7"],["/categories/数据结构和算法/算法题/二分查找/index.html","2560355291f1770fdcbc0f106863177e"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","76c87c86046738d9e017273da8ccf5b1"],["/categories/数据结构和算法/算法题/动态规划/index.html","6c1da89401566b56ccdad834bdc4dadb"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","8f906a529585aa79ea8bf14e5acc2f31"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","02d45f45593559c614ed6fbd434df0ee"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","4e8ac44e246bb125e9209a57011ecfc6"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","11332dde620b72541e7cdeb9abbebf55"],["/categories/数据结构和算法/算法题/数论/index.html","d94aa0ae69ae99db0e61190d5e69aec0"],["/categories/数据结构和算法/算法题/栈和队列/index.html","5bc1eb60df511280b819e44322bae8a0"],["/categories/数据结构和算法/算法题/树论/index.html","2a01e56c5a0216f7bb3855141b2b6c50"],["/categories/杂七杂八/index.html","3b650bd510eaaa3c30026fd5878c5256"],["/categories/杂七杂八/博客搭建/index.html","046d348f86f2892a8bae252eb8aa4e78"],["/categories/编程工具下载/index.html","c485004f4e1d525db913044eb0464132"],["/categories/编程环境/index.html","a78fbdaf30707bee7b2c537832bcce35"],["/categories/编程环境/大数据/index.html","49780ce390a62d2e195e0524559753d1"],["/categories/英语学习/index.html","2dc7298654420e78711af3c560c06fcf"],["/categories/英语学习/英语语法/index.html","12bf3d1ee71cf06794dcf095d0feab04"],["/comments/index.html","a81c45a8228ca8879558b9e1b061aa96"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","9cc53e61e4b302d691215cfb604caf4f"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","aa3b48e8a34e8fa10c1c4cecaceb4fb4"],["/movies/index.html","089bcdcf214f743dc227df019ce10ea3"],["/music/index.html","c4d35d3ab541f1df445c26666ba01873"],["/page/2/index.html","3a2e58997ffb066c7f3457ebe219dff2"],["/page/3/index.html","bd35ca1d6841912313e0f6c945377f3d"],["/page/4/index.html","3a6cdc97a1304f0d376246c611961b76"],["/page/5/index.html","dd446394ce9185c07ba0c555ace8315c"],["/page/6/index.html","c62e6a0e3bfff18535388bde0fd639a0"],["/posts/1021360842.html","cfce8ef8f4e0fe9b2d08f7df87ee3135"],["/posts/1120620192.html","c3599b7b4e3fd3a9c01da4c2c85373d7"],["/posts/1141628095.html","89fcd94034e757006ccddb131946e497"],["/posts/1168613674.html","33fd0cd391e70cf2943e226ca47aa81d"],["/posts/1219920510.html","504079f2d1079ffb8feb82c6cbc95d88"],["/posts/1222166338.html","a3ef78f65ba3275456269c9733d4a848"],["/posts/1259097482.html","de1a54932815cab4950aaccaf46df507"],["/posts/1271036369.html","e3d793938ac58f7371ecd19039eadf12"],["/posts/1312847445.html","3770bd7fd11c9391cf51e4ec6d7ad74b"],["/posts/135355774.html","e31298bd48c3870318522f30ba6bd5e4"],["/posts/1375344716.html","47a33993a38c45661dd57da8586c7bc2"],["/posts/1388991698.html","cd469849394749c1ade1eb46223440d2"],["/posts/1410315814.html","db1173e103bc89447e476aed378dcf36"],["/posts/1452790229.html","798a69cd61fa93fee767aec9e06c1710"],["/posts/1470079884.html","1f6f9bad70da58e037d9b83a2f37286f"],["/posts/1470079885.html","d5db975f3366dfe0efbe589fb2a5a64f"],["/posts/1470079886.html","aedfd872c93e901f0e3ab902515338eb"],["/posts/1470079887.html","0fac9d96b4f44cff56a7a6127470ad9f"],["/posts/1498536549.html","c4d83e1d98dfd46e66c5a9c90e01c70a"],["/posts/1539568593.html","519aa8f39d3c8111bf80df456e4466f5"],["/posts/1547067935.html","47923a4bcc4e30d25d6d191e8c260021"],["/posts/1557866301.html","99540a8895793911b208786db0314ea2"],["/posts/1571776361.html","622350e770a4a50810b99757ea9d2d24"],["/posts/1605124548.html","bbe8daa8040134f961a51f8d7747a0ea"],["/posts/1633036852.html","0b25e68f5457806f188d8debb2439bac"],["/posts/1674202625.html","93d93093c2dd47edfd5010c135c44aae"],["/posts/1765123828.html","be06e586dd92fa93dd489ef8c9d31686"],["/posts/1767336200.html","e143fe0c6a8f5dfd5a2bc56925d66534"],["/posts/1776114197.html","84808e92c91b1c304e8b9d1e224b419f"],["/posts/1817748743.html","eb64e7efbaea4ff8aeb4f84fd3f44189"],["/posts/1925125395.html","86352bdb51557ef8e53fdfb99224a03e"],["/posts/1966191251.html","fcc166c61c5699c21ab1528882b984f7"],["/posts/1987617322.html","a70692f97ebdc9db4aab9e4e9b94a905"],["/posts/1999788039.html","7f36f54766989f3d030a8e0a6263b268"],["/posts/2075104059.html","219904f5568a26dfad7f2b94198e994a"],["/posts/2087796737.html","cdb3b806434a7d6474b35aa4f6d35064"],["/posts/2106547339.html","c1858c7b9a6350a530854554ef9351cd"],["/posts/2207806286.html","f13f0f711377951180dcfd7dddf52ef4"],["/posts/2225903441.html","2d8336c8722e75ce30a7e94e3379a486"],["/posts/2265610284.html","c0f3a2fd3d3391e1b3a512b3c1f580b8"],["/posts/2281352001.html","ba9d6e4e82a0387c90c361c5cf102d9e"],["/posts/2364755265.html","b6ab76d98800cd62518d22518ae27d2f"],["/posts/2414116852.html","cc9277920ff4af188c0060e80607e895"],["/posts/2421785022.html","66612503e3f28818fb0c24f0d609e282"],["/posts/2482902029.html","9a6d597914e9bccacd8f1e9d60c9164e"],["/posts/2495386210.html","89382af0af39c42a450ef9326ac80f8d"],["/posts/2516528882.html","e219a47e67237ba696210579f7e27b7f"],["/posts/2526659543.html","0cd20c8b19d10bb9e402939897961450"],["/posts/2529807823.html","8fb360cb16ec46143defd99b8941b600"],["/posts/2596601004.html","809b0666d296107988698d6782d2aab0"],["/posts/2697614349.html","3d6ded0f747f5993bec76796e6eef4db"],["/posts/2742438348.html","43264a8b5d3be175a3fef243d8a99ede"],["/posts/2768249503.html","749c332f066300fdc2559cc205cc07eb"],["/posts/2864584994.html","e22951857ed07d4eadf6164d49f5e8d4"],["/posts/2888309600.html","caa66860965c187677f1395336e21799"],["/posts/2891591958.html","78ec2318f0b064a9f5526ec5063fd6ab"],["/posts/2909934084.html","5d9fd3c76411dede9ae48af6fe584fad"],["/posts/2920256992.html","cfed75d9b6bd02d5f7308b9e515fbc62"],["/posts/2959474469.html","06e7857b6904d4755e397fa9ab256686"],["/posts/3005926051.html","e0a9f0dd2c04ba01b11be13ee0cd3d02"],["/posts/309775400.html","5058685f2fcc56c90d2b37cf1a76b0fd"],["/posts/3156194925.html","e25c152719562ed03cfca40eaaea51a3"],["/posts/3169224211.html","66a34ae5a3828e5456cda7444afb305a"],["/posts/3213899550.html","e07cd7a9f1c467175f33d3b7cc70a8c3"],["/posts/3259212833.html","169c8a89cc151301a8880b227dd823ea"],["/posts/3266130344.html","db555a8d23537ad410260617d66a441b"],["/posts/3292663995.html","5ce782ccce334649d5705ded604bc2cd"],["/posts/3297135020.html","e8a86e3e1f76c36b107d2f1eda7f3080"],["/posts/3306641566.html","2677e6e360af3cce66709e72fbe8e832"],["/posts/3312011324.html","922da5d2031a32b13e9247a356ac77d4"],["/posts/336911618.html","16e04b29c7870b69816768330611fd33"],["/posts/3402121571.html","de8678027b79ccb7bcdb061e9ede3900"],["/posts/3405577485.html","be9c748ca34e4f46a330d480cf4fbcce"],["/posts/3498516849.html","83b266b78207ca214f9da208ca300482"],["/posts/3513711414.html","aec69379bc57fffe116704f102ce4235"],["/posts/3523095624.html","a72f3153dc9412c5898737c4662f833e"],["/posts/3546711884.html","cd7584546c379a2e5d197c8baf45fdd5"],["/posts/3731385230.html","84d62b543310125534a4412501ada4ba"],["/posts/3772089482.html","5b718d1b752c15e418470463161fb3f6"],["/posts/386609427.html","97d8ec0c0fa13e3fb37dbb49ceac5504"],["/posts/4044235327.html","1dba9d573a4e197ac9cb7710efe62835"],["/posts/4115971639.html","0380699f740463c232898b3e1d69f9fa"],["/posts/4130790367.html","6f666e1652183d5038d6a36d2ae0e71d"],["/posts/4131986683.html","410e9eaefbed441f4dc9d0e659c94337"],["/posts/4177218757.html","7ccd25ca4c634e335aad29c4711d1642"],["/posts/4192183953.html","7e3ee999311f5a797638a1680900e566"],["/posts/4261103898.html","fdfa6594514d531659b92b2ea5a8f303"],["/posts/469711973.html","04d606a99c8f2c6b8bdce1e749becca2"],["/posts/482495853.html","254722e4277390b1409d0bdc96492d37"],["/posts/488247922.html","e9d04ededd1dff94e850d5734027fc72"],["/posts/517302816.html","caebff87d1c924085b5438a3b52f9215"],["/posts/570165348.html","001158c8fc5e96590c18990e2e2da214"],["/posts/595890772.html","9649b5a790a43dd9e52c05c74bde6ea0"],["/posts/67485572.html","be894b9415cb3204508995f024b86651"],["/posts/694347442.html","9c3d4a72af828b216d395bcdf6b8ad97"],["/posts/707384687.html","101eab0523abed7b4501fdcfcfc841b5"],["/posts/71180092.html","7563b85ca497a2daa6f5aa19bc1b61ef"],["/posts/716459272.html","e37e43be67fc3461eb41f095f53433a0"],["/posts/765481613.html","d01a4fd8edcb1c76dd57489797e80696"],["/posts/778231993.html","386df582beead2f7d94e2c321503d991"],["/posts/795397410.html","40ccddc2bb6206d7b31d819e1aefb28b"],["/posts/820223701.html","53ec7f6e2b35db65aef250e3a35721fe"],["/posts/830372185.html","3e346222f640c6f8cb63495c67b8d96a"],["/posts/88294277.html","17cc7e3e5d33a3c8de9cd6b38e434f04"],["/posts/939963535.html","6c0dc51d3b17c34a6f54798486ef291a"],["/posts/983786067.html","c3e85bbe73277f9c082b12167b839ef5"],["/sw-register.js","0c4ed0ceb537f70ccff2b1e7429687c0"],["/tags/C/index.html","7341bb6d6e5ab92bd9b8d64c23fc5dc9"],["/tags/C/page/2/index.html","69a70e1c2bc47c1efb6fee043f114c69"],["/tags/C/page/3/index.html","e6463414e49c511cccf7476fb9532820"],["/tags/C/page/4/index.html","0a886c79f09c45d1761678d13aaf1c49"],["/tags/ETL/index.html","93c010d418b37ed125963047170ff202"],["/tags/ElasticSearch/index.html","95e63d85faee827bd87885b76fbd0c2b"],["/tags/GUI/index.html","6032cd4f6d153bd0c30b728c5400f0c9"],["/tags/HBase/index.html","956e460d8e23f87cb6e500e537704eb0"],["/tags/Hadoop/index.html","4dafabf7dc00029c8bdadc99bd1945e1"],["/tags/Hadoop/page/2/index.html","ff943db4e07152c5f7106d86d4f4f88b"],["/tags/Java/index.html","6436c353078d0257a12843c105483933"],["/tags/Java后端/index.html","da9c230e4eb9be6dbf512246a71c123f"],["/tags/Java后端/page/2/index.html","cfdebe405bcf44e6f705c891556850b0"],["/tags/Java基础/index.html","67a5d80292c84f8292c6f42757ae5341"],["/tags/Java基础/page/2/index.html","9e2a2f6c8b2c2f08a224b1e8c1bd153e"],["/tags/Kettle/index.html","3d433ccf263ccabd10a889475b34b833"],["/tags/Kibana/index.html","901d3ce562b7c0e18fa56e11462404a2"],["/tags/Linux/index.html","7c3a291b6033481a0bcb5e076d3056e5"],["/tags/Linux/page/2/index.html","0f12fa6cbc4c13a8bee9af709cce2138"],["/tags/Linux/page/3/index.html","32af5a2d0caddf9d31aba0c9407fa67e"],["/tags/Mac/index.html","b433b3c4b97d2436cb94a7ab5f6b94e7"],["/tags/Mac/page/2/index.html","6cd892dab42b7f4fb43b99230a731a83"],["/tags/Maven/index.html","c0deab30cfa05ca3c006f62e7f63ea9a"],["/tags/MySQL/index.html","eece8417f26daaf3ba18d43bb9879efe"],["/tags/Python/index.html","5a0fdf5b7983ad503e39112bc3e8219b"],["/tags/Redis/index.html","79aeeccc1e6316bc0a8c85ac60799d3e"],["/tags/R语言/index.html","2a8928713395d24ba322ba017d9be494"],["/tags/Spark/index.html","0cec1a67e53e583a5e72abb67f1f0869"],["/tags/Ubuntu/index.html","fa45c4481bd39b3a0d9df763a629189b"],["/tags/Vue/index.html","da91dcf894f6d4a4a26541a8a0520f3a"],["/tags/Windows/index.html","50995b7eb4fd6e3f33921ecdebccda52"],["/tags/ZooKeeper/index.html","925eef74a27b48ec895f87db762cf8aa"],["/tags/bfs/index.html","37f346b4e4d10a63c29839017736ed50"],["/tags/dfs/index.html","23fcf223e947278275f0ea8b7247ccc4"],["/tags/folium/index.html","8d0cb460a17c827c93840d785e674ab5"],["/tags/git/index.html","614d337fad66854c4c9dd817675e7030"],["/tags/index.html","659f50a4f009f5d365b9b08d24e9e043"],["/tags/latex/index.html","126879e9f5060a8a3031acf14fb7d4b3"],["/tags/中间件/index.html","60b762c8448ff2810d2f7f5082ffd0b0"],["/tags/二分查找/index.html","2342ca7d926323889af91a833a4fb1c9"],["/tags/优化类/index.html","eff047e9b3b4fbb9cd55d654a307caaa"],["/tags/前端/index.html","0ed77ab5f57c51a8f4829fde868039bd"],["/tags/前缀和与差分/index.html","3af2fc282a026a42cf880f141c04ccf5"],["/tags/动态规划/index.html","50a75f553a609b3ac1bcea860984feae"],["/tags/动态规划/page/2/index.html","622fc797a1e2efeb03e129ea14c14597"],["/tags/博客搭建/index.html","1efa9f12d0bd659e3ff0f6905c2d086f"],["/tags/图论/index.html","a3881c3b3b5cb2e8c4623a41d584d5b9"],["/tags/大数据/index.html","da7ca53584b5cc4bbda426a2d36549af"],["/tags/大数据/page/2/index.html","23c754481e748c2a32ecfbb965afe9c1"],["/tags/操作系统/index.html","5ce2a7859de55c141923f0e14fcb6878"],["/tags/数学建模/index.html","62ad855469652463e677073f4b682eb9"],["/tags/数据库/index.html","7d62f6ee704ab49cd92a84a78c690bcf"],["/tags/数据结构和算法/index.html","407163dc343a712dddde943d6b19c027"],["/tags/数据结构和算法/page/2/index.html","07c1b5b55777d2b09c791d016ab43987"],["/tags/数据结构和算法/page/3/index.html","99af6e8b6336171da67556451a894bf7"],["/tags/数据结构和算法/page/4/index.html","26c4add89587b5aabeedd2eab5955b6b"],["/tags/数组和字符串/index.html","cdb311b91f2fb908f589235b9959e5df"],["/tags/数论/index.html","d223b50eccab7b22d3315a63041199ac"],["/tags/枚举类/index.html","1ac5a0c76ad3e15c5f126d6b926dea07"],["/tags/栈和队列/index.html","4fe66106e618c4f80162c994d7bd5e5c"],["/tags/树论/index.html","3163dc76982c191220d48ae41172d84f"],["/tags/测试/index.html","929c403e598d62c56613a2611ee5a1fa"],["/tags/环境/index.html","67d9f5037038f8a5be01b4607b98fc9b"],["/tags/环境变量/index.html","91a0557aa22a0fc4d06c56fe252434d3"],["/tags/绘图/index.html","fb06877beb0b0d9f306ce23e37aeafd8"],["/tags/编程工具/index.html","385a9a0cc2aa29aae9c1af0bf29346ac"],["/tags/编程环境/index.html","a77d6830a0636cefbdf2a141dd37201e"],["/tags/网络编程/index.html","7582023015ebae9461f4956f88bfe1a1"],["/tags/英语语法/index.html","76de8550bae73e0e9a646449e52a902c"],["/tags/计算机操作系统/index.html","1a303f46f2efe34edda203c5db255d19"],["/tags/论文/index.html","b0ef0258b91a12bfc522fd4b8efcc23b"],["/tags/资源下载/index.html","66d3ef29bd3687c7bcac3ffef8cd7dae"],["/tags/链表/index.html","6ba5c138ee3287b91936816d370567e3"],["/tags/集合/index.html","5e70e5ef44f265707c640112c313ed48"],["/tags/集群/index.html","11ad3191acd3f29635597ba5e66af556"]];
var cacheName = 'sw-precache-v3--' + (self.registration ? self.registration.scope : '');
var firstRegister = 1; // 默认1是首次安装SW， 0是SW更新


var ignoreUrlParametersMatching = [/^utm_/];


var addDirectoryIndex = function (originalUrl, index) {
    var url = new URL(originalUrl);
    if (url.pathname.slice(-1) === '/') {
        url.pathname += index;
    }
    return url.toString();
};

var cleanResponse = function (originalResponse) {
    // 如果没有重定向响应，不需干啥
    if (!originalResponse.redirected) {
        return Promise.resolve(originalResponse);
    }

    // Firefox 50 及以下不知处 Response.body 流, 所以我们需要读取整个body以blob形式返回。
    var bodyPromise = 'body' in originalResponse ?
        Promise.resolve(originalResponse.body) :
        originalResponse.blob();

    return bodyPromise.then(function (body) {
        // new Response() 可同时支持 stream or Blob.
        return new Response(body, {
            headers: originalResponse.headers,
            status: originalResponse.status,
            statusText: originalResponse.statusText
        });
    });
};

var createCacheKey = function (originalUrl, paramName, paramValue,
    dontCacheBustUrlsMatching) {

    // 创建一个新的URL对象，避免影响原始URL
    var url = new URL(originalUrl);

    // 如果 dontCacheBustUrlsMatching 值没有设置，或是没有匹配到，将值拼接到url.serach后
    if (!dontCacheBustUrlsMatching ||
        !(url.pathname.match(dontCacheBustUrlsMatching))) {
        url.search += (url.search ? '&' : '') +
            encodeURIComponent(paramName) + '=' + encodeURIComponent(paramValue);
    }

    return url.toString();
};

var isPathWhitelisted = function (whitelist, absoluteUrlString) {
    // 如果 whitelist 是空数组，则认为全部都在白名单内
    if (whitelist.length === 0) {
        return true;
    }

    // 否则逐个匹配正则匹配并返回
    var path = (new URL(absoluteUrlString)).pathname;
    return whitelist.some(function (whitelistedPathRegex) {
        return path.match(whitelistedPathRegex);
    });
};

var stripIgnoredUrlParameters = function (originalUrl,
    ignoreUrlParametersMatching) {
    var url = new URL(originalUrl);
    // 移除 hash; 查看 https://github.com/GoogleChrome/sw-precache/issues/290
    url.hash = '';

    url.search = url.search.slice(1) // 是否包含 '?'
        .split('&') // 分割成数组 'key=value' 的形式
        .map(function (kv) {
            return kv.split('='); // 分割每个 'key=value' 字符串成 [key, value] 形式
        })
        .filter(function (kv) {
            return ignoreUrlParametersMatching.every(function (ignoredRegex) {
                return !ignoredRegex.test(kv[0]); // 如果 key 没有匹配到任何忽略参数正则，就 Return true
            });
        })
        .map(function (kv) {
            return kv.join('='); // 重新把 [key, value] 格式转换为 'key=value' 字符串
        })
        .join('&'); // 将所有参数 'key=value' 以 '&' 拼接

    return url.toString();
};


var addDirectoryIndex = function (originalUrl, index) {
    var url = new URL(originalUrl);
    if (url.pathname.slice(-1) === '/') {
        url.pathname += index;
    }
    return url.toString();
};

var hashParamName = '_sw-precache';
var urlsToCacheKeys = new Map(
    precacheConfig.map(function (item) {
        var relativeUrl = item[0];
        var hash = item[1];
        var absoluteUrl = new URL(relativeUrl, self.location);
        var cacheKey = createCacheKey(absoluteUrl, hashParamName, hash, false);
        return [absoluteUrl.toString(), cacheKey];
    })
);

function setOfCachedUrls(cache) {
    return cache.keys().then(function (requests) {
        // 如果原cacheName中没有缓存任何收，就默认是首次安装，否则认为是SW更新
        if (requests && requests.length > 0) {
            firstRegister = 0; // SW更新
        }
        return requests.map(function (request) {
            return request.url;
        });
    }).then(function (urls) {
        return new Set(urls);
    });
}

self.addEventListener('install', function (event) {
    event.waitUntil(
        caches.open(cacheName).then(function (cache) {
            return setOfCachedUrls(cache).then(function (cachedUrls) {
                return Promise.all(
                    Array.from(urlsToCacheKeys.values()).map(function (cacheKey) {
                        // 如果缓存中没有匹配到cacheKey，添加进去
                        if (!cachedUrls.has(cacheKey)) {
                            var request = new Request(cacheKey, { credentials: 'same-origin' });
                            return fetch(request).then(function (response) {
                                // 只要返回200才能继续，否则直接抛错
                                if (!response.ok) {
                                    throw new Error('Request for ' + cacheKey + ' returned a ' +
                                        'response with status ' + response.status);
                                }

                                return cleanResponse(response).then(function (responseToCache) {
                                    return cache.put(cacheKey, responseToCache);
                                });
                            });
                        }
                    })
                );
            });
        })
            .then(function () {
            
            // 强制 SW 状态 installing -> activate
            return self.skipWaiting();
            
        })
    );
});

self.addEventListener('activate', function (event) {
    var setOfExpectedUrls = new Set(urlsToCacheKeys.values());

    event.waitUntil(
        caches.open(cacheName).then(function (cache) {
            return cache.keys().then(function (existingRequests) {
                return Promise.all(
                    existingRequests.map(function (existingRequest) {
                        // 删除原缓存中相同键值内容
                        if (!setOfExpectedUrls.has(existingRequest.url)) {
                            return cache.delete(existingRequest);
                        }
                    })
                );
            });
        }).then(function () {
            
            return self.clients.claim();
            
        }).then(function () {
                // 如果是首次安装 SW 时, 不发送更新消息（是否是首次安装，通过指定cacheName 中是否有缓存信息判断）
                // 如果不是首次安装，则是内容有更新，需要通知页面重载更新
                if (!firstRegister) {
                    return self.clients.matchAll()
                        .then(function (clients) {
                            if (clients && clients.length) {
                                clients.forEach(function (client) {
                                    client.postMessage('sw.update');
                                })
                            }
                        })
                }
            })
    );
});



    self.addEventListener('fetch', function (event) {
        if (event.request.method === 'GET') {

            // 是否应该 event.respondWith()，需要我们逐步的判断
            // 而且也方便了后期做特殊的特殊
            var shouldRespond;


            // 首先去除已配置的忽略参数及hash
            // 查看缓存简直中是否包含该请求，包含就将shouldRespond 设为true
            var url = stripIgnoredUrlParameters(event.request.url, ignoreUrlParametersMatching);
            shouldRespond = urlsToCacheKeys.has(url);

            // 如果 shouldRespond 是 false, 我们在url后默认增加 'index.html'
            // (或者是你在配置文件中自行配置的 directoryIndex 参数值)，继续查找缓存列表
            var directoryIndex = 'index.html';
            if (!shouldRespond && directoryIndex) {
                url = addDirectoryIndex(url, directoryIndex);
                shouldRespond = urlsToCacheKeys.has(url);
            }

            // 如果 shouldRespond 仍是 false，检查是否是navigation
            // request， 如果是的话，判断是否能与 navigateFallbackWhitelist 正则列表匹配
            var navigateFallback = '';
            if (!shouldRespond &&
                navigateFallback &&
                (event.request.mode === 'navigate') &&
                isPathWhitelisted([], event.request.url)
            ) {
                url = new URL(navigateFallback, self.location).toString();
                shouldRespond = urlsToCacheKeys.has(url);
            }

            // 如果 shouldRespond 被置为 true
            // 则 event.respondWith()匹配缓存返回结果，匹配不成就直接请求.
            if (shouldRespond) {
                event.respondWith(
                    caches.open(cacheName).then(function (cache) {
                        return cache.match(urlsToCacheKeys.get(url)).then(function (response) {
                            if (response) {
                                return response;
                            }
                            throw Error('The cached response that was expected is missing.');
                        });
                    }).catch(function (e) {
                        // 如果捕获到异常错误，直接返回 fetch() 请求资源
                        console.warn('Couldn\'t serve response for "%s" from cache: %O', event.request.url, e);
                        return fetch(event.request);
                    })
                );
            }
        }
    });



// *** Start of auto-included sw-toolbox code. ***
/* 
 Copyright 2016 Google Inc. All Rights Reserved.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/!function(e){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=e();else if("function"==typeof define&&define.amd)define([],e);else{var t;t="undefined"!=typeof window?window:"undefined"!=typeof global?global:"undefined"!=typeof self?self:this,t.toolbox=e()}}(function(){return function e(t,n,r){function o(c,s){if(!n[c]){if(!t[c]){var a="function"==typeof require&&require;if(!s&&a)return a(c,!0);if(i)return i(c,!0);var u=new Error("Cannot find module '"+c+"'");throw u.code="MODULE_NOT_FOUND",u}var f=n[c]={exports:{}};t[c][0].call(f.exports,function(e){var n=t[c][1][e];return o(n?n:e)},f,f.exports,e,t,n,r)}return n[c].exports}for(var i="function"==typeof require&&require,c=0;c<r.length;c++)o(r[c]);return o}({1:[function(e,t,n){"use strict";function r(e,t){t=t||{};var n=t.debug||m.debug;n&&console.log("[sw-toolbox] "+e)}function o(e){var t;return e&&e.cache&&(t=e.cache.name),t=t||m.cache.name,caches.open(t)}function i(e,t){t=t||{};var n=t.successResponses||m.successResponses;return fetch(e.clone()).then(function(r){return"GET"===e.method&&n.test(r.status)&&o(t).then(function(n){n.put(e,r).then(function(){var r=t.cache||m.cache;(r.maxEntries||r.maxAgeSeconds)&&r.name&&c(e,n,r)})}),r.clone()})}function c(e,t,n){var r=s.bind(null,e,t,n);d=d?d.then(r):r()}function s(e,t,n){var o=e.url,i=n.maxAgeSeconds,c=n.maxEntries,s=n.name,a=Date.now();return r("Updating LRU order for "+o+". Max entries is "+c+", max age is "+i),g.getDb(s).then(function(e){return g.setTimestampForUrl(e,o,a)}).then(function(e){return g.expireEntries(e,c,i,a)}).then(function(e){r("Successfully updated IDB.");var n=e.map(function(e){return t.delete(e)});return Promise.all(n).then(function(){r("Done with cache cleanup.")})}).catch(function(e){r(e)})}function a(e,t,n){return r("Renaming cache: ["+e+"] to ["+t+"]",n),caches.delete(t).then(function(){return Promise.all([caches.open(e),caches.open(t)]).then(function(t){var n=t[0],r=t[1];return n.keys().then(function(e){return Promise.all(e.map(function(e){return n.match(e).then(function(t){return r.put(e,t)})}))}).then(function(){return caches.delete(e)})})})}function u(e,t){return o(t).then(function(t){return t.add(e)})}function f(e,t){return o(t).then(function(t){return t.delete(e)})}function h(e){e instanceof Promise||p(e),m.preCacheItems=m.preCacheItems.concat(e)}function p(e){var t=Array.isArray(e);if(t&&e.forEach(function(e){"string"==typeof e||e instanceof Request||(t=!1)}),!t)throw new TypeError("The precache method expects either an array of strings and/or Requests or a Promise that resolves to an array of strings and/or Requests.");return e}function l(e,t,n){if(!e)return!1;if(t){var r=e.headers.get("date");if(r){var o=new Date(r);if(o.getTime()+1e3*t<n)return!1}}return!0}var d,m=e("./options"),g=e("./idb-cache-expiration");t.exports={debug:r,fetchAndCache:i,openCache:o,renameCache:a,cache:u,uncache:f,precache:h,validatePrecacheInput:p,isResponseFresh:l}},{"./idb-cache-expiration":2,"./options":4}],2:[function(e,t,n){"use strict";function r(e){return new Promise(function(t,n){var r=indexedDB.open(u+e,f);r.onupgradeneeded=function(){var e=r.result.createObjectStore(h,{keyPath:p});e.createIndex(l,l,{unique:!1})},r.onsuccess=function(){t(r.result)},r.onerror=function(){n(r.error)}})}function o(e){return e in d||(d[e]=r(e)),d[e]}function i(e,t,n){return new Promise(function(r,o){var i=e.transaction(h,"readwrite"),c=i.objectStore(h);c.put({url:t,timestamp:n}),i.oncomplete=function(){r(e)},i.onabort=function(){o(i.error)}})}function c(e,t,n){return t?new Promise(function(r,o){var i=1e3*t,c=[],s=e.transaction(h,"readwrite"),a=s.objectStore(h),u=a.index(l);u.openCursor().onsuccess=function(e){var t=e.target.result;if(t&&n-i>t.value[l]){var r=t.value[p];c.push(r),a.delete(r),t.continue()}},s.oncomplete=function(){r(c)},s.onabort=o}):Promise.resolve([])}function s(e,t){return t?new Promise(function(n,r){var o=[],i=e.transaction(h,"readwrite"),c=i.objectStore(h),s=c.index(l),a=s.count();s.count().onsuccess=function(){var e=a.result;e>t&&(s.openCursor().onsuccess=function(n){var r=n.target.result;if(r){var i=r.value[p];o.push(i),c.delete(i),e-o.length>t&&r.continue()}})},i.oncomplete=function(){n(o)},i.onabort=r}):Promise.resolve([])}function a(e,t,n,r){return c(e,n,r).then(function(n){return s(e,t).then(function(e){return n.concat(e)})})}var u="sw-toolbox-",f=1,h="store",p="url",l="timestamp",d={};t.exports={getDb:o,setTimestampForUrl:i,expireEntries:a}},{}],3:[function(e,t,n){"use strict";function r(e){var t=a.match(e.request);t?e.respondWith(t(e.request)):a.default&&"GET"===e.request.method&&0===e.request.url.indexOf("http")&&e.respondWith(a.default(e.request))}function o(e){s.debug("activate event fired");var t=u.cache.name+"$$$inactive$$$";e.waitUntil(s.renameCache(t,u.cache.name))}function i(e){return e.reduce(function(e,t){return e.concat(t)},[])}function c(e){var t=u.cache.name+"$$$inactive$$$";s.debug("install event fired"),s.debug("creating cache ["+t+"]"),e.waitUntil(s.openCache({cache:{name:t}}).then(function(e){return Promise.all(u.preCacheItems).then(i).then(s.validatePrecacheInput).then(function(t){return s.debug("preCache list: "+(t.join(", ")||"(none)")),e.addAll(t)})}))}e("serviceworker-cache-polyfill");var s=e("./helpers"),a=e("./router"),u=e("./options");t.exports={fetchListener:r,activateListener:o,installListener:c}},{"./helpers":1,"./options":4,"./router":6,"serviceworker-cache-polyfill":16}],4:[function(e,t,n){"use strict";var r;r=self.registration?self.registration.scope:self.scope||new URL("./",self.location).href,t.exports={cache:{name:"$$$toolbox-cache$$$"+r+"$$$",maxAgeSeconds:null,maxEntries:null},debug:!1,networkTimeoutSeconds:null,preCacheItems:[],successResponses:/^0|([123]\d\d)|(40[14567])|410$/}},{}],5:[function(e,t,n){"use strict";var r=new URL("./",self.location),o=r.pathname,i=e("path-to-regexp"),c=function(e,t,n,r){t instanceof RegExp?this.fullUrlRegExp=t:(0!==t.indexOf("/")&&(t=o+t),this.keys=[],this.regexp=i(t,this.keys)),this.method=e,this.options=r,this.handler=n};c.prototype.makeHandler=function(e){var t;if(this.regexp){var n=this.regexp.exec(e);t={},this.keys.forEach(function(e,r){t[e.name]=n[r+1]})}return function(e){return this.handler(e,t,this.options)}.bind(this)},t.exports=c},{"path-to-regexp":15}],6:[function(e,t,n){"use strict";function r(e){return e.replace(/[-\/\\^$*+?.()|[\]{}]/g,"\\$&")}var o=e("./route"),i=e("./helpers"),c=function(e,t){for(var n=e.entries(),r=n.next(),o=[];!r.done;){var i=new RegExp(r.value[0]);i.test(t)&&o.push(r.value[1]),r=n.next()}return o},s=function(){this.routes=new Map,this.routes.set(RegExp,new Map),this.default=null};["get","post","put","delete","head","any"].forEach(function(e){s.prototype[e]=function(t,n,r){return this.add(e,t,n,r)}}),s.prototype.add=function(e,t,n,c){c=c||{};var s;t instanceof RegExp?s=RegExp:(s=c.origin||self.location.origin,s=s instanceof RegExp?s.source:r(s)),e=e.toLowerCase();var a=new o(e,t,n,c);this.routes.has(s)||this.routes.set(s,new Map);var u=this.routes.get(s);u.has(e)||u.set(e,new Map);var f=u.get(e),h=a.regexp||a.fullUrlRegExp;f.has(h.source)&&i.debug('"'+t+'" resolves to same regex as existing route.'),f.set(h.source,a)},s.prototype.matchMethod=function(e,t){var n=new URL(t),r=n.origin,o=n.pathname;return this._match(e,c(this.routes,r),o)||this._match(e,[this.routes.get(RegExp)],t)},s.prototype._match=function(e,t,n){if(0===t.length)return null;for(var r=0;r<t.length;r++){var o=t[r],i=o&&o.get(e.toLowerCase());if(i){var s=c(i,n);if(s.length>0)return s[0].makeHandler(n)}}return null},s.prototype.match=function(e){return this.matchMethod(e.method,e.url)||this.matchMethod("any",e.url)},t.exports=new s},{"./helpers":1,"./route":5}],7:[function(e,t,n){"use strict";function r(e,t,n){return n=n||{},i.debug("Strategy: cache first ["+e.url+"]",n),i.openCache(n).then(function(t){return t.match(e).then(function(t){var r=n.cache||o.cache,c=Date.now();return i.isResponseFresh(t,r.maxAgeSeconds,c)?t:i.fetchAndCache(e,n)})})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],8:[function(e,t,n){"use strict";function r(e,t,n){return n=n||{},i.debug("Strategy: cache only ["+e.url+"]",n),i.openCache(n).then(function(t){return t.match(e).then(function(e){var t=n.cache||o.cache,r=Date.now();if(i.isResponseFresh(e,t.maxAgeSeconds,r))return e})})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],9:[function(e,t,n){"use strict";function r(e,t,n){return o.debug("Strategy: fastest ["+e.url+"]",n),new Promise(function(r,c){var s=!1,a=[],u=function(e){a.push(e.toString()),s?c(new Error('Both cache and network failed: "'+a.join('", "')+'"')):s=!0},f=function(e){e instanceof Response?r(e):u("No result returned")};o.fetchAndCache(e.clone(),n).then(f,u),i(e,t,n).then(f,u)})}var o=e("../helpers"),i=e("./cacheOnly");t.exports=r},{"../helpers":1,"./cacheOnly":8}],10:[function(e,t,n){t.exports={networkOnly:e("./networkOnly"),networkFirst:e("./networkFirst"),cacheOnly:e("./cacheOnly"),cacheFirst:e("./cacheFirst"),fastest:e("./fastest")}},{"./cacheFirst":7,"./cacheOnly":8,"./fastest":9,"./networkFirst":11,"./networkOnly":12}],11:[function(e,t,n){"use strict";function r(e,t,n){n=n||{};var r=n.successResponses||o.successResponses,c=n.networkTimeoutSeconds||o.networkTimeoutSeconds;return i.debug("Strategy: network first ["+e.url+"]",n),i.openCache(n).then(function(t){var s,a,u=[];if(c){var f=new Promise(function(r){s=setTimeout(function(){t.match(e).then(function(e){var t=n.cache||o.cache,c=Date.now(),s=t.maxAgeSeconds;i.isResponseFresh(e,s,c)&&r(e)})},1e3*c)});u.push(f)}var h=i.fetchAndCache(e,n).then(function(e){if(s&&clearTimeout(s),r.test(e.status))return e;throw i.debug("Response was an HTTP error: "+e.statusText,n),a=e,new Error("Bad response")}).catch(function(r){return i.debug("Network or response error, fallback to cache ["+e.url+"]",n),t.match(e).then(function(e){if(e)return e;if(a)return a;throw r})});return u.push(h),Promise.race(u)})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],12:[function(e,t,n){"use strict";function r(e,t,n){return o.debug("Strategy: network only ["+e.url+"]",n),fetch(e)}var o=e("../helpers");t.exports=r},{"../helpers":1}],13:[function(e,t,n){"use strict";var r=e("./options"),o=e("./router"),i=e("./helpers"),c=e("./strategies"),s=e("./listeners");i.debug("Service Worker Toolbox is loading"),self.addEventListener("install",s.installListener),self.addEventListener("activate",s.activateListener),self.addEventListener("fetch",s.fetchListener),t.exports={networkOnly:c.networkOnly,networkFirst:c.networkFirst,cacheOnly:c.cacheOnly,cacheFirst:c.cacheFirst,fastest:c.fastest,router:o,options:r,cache:i.cache,uncache:i.uncache,precache:i.precache}},{"./helpers":1,"./listeners":3,"./options":4,"./router":6,"./strategies":10}],14:[function(e,t,n){t.exports=Array.isArray||function(e){return"[object Array]"==Object.prototype.toString.call(e)}},{}],15:[function(e,t,n){function r(e,t){for(var n,r=[],o=0,i=0,c="",s=t&&t.delimiter||"/";null!=(n=x.exec(e));){var f=n[0],h=n[1],p=n.index;if(c+=e.slice(i,p),i=p+f.length,h)c+=h[1];else{var l=e[i],d=n[2],m=n[3],g=n[4],v=n[5],w=n[6],y=n[7];c&&(r.push(c),c="");var b=null!=d&&null!=l&&l!==d,E="+"===w||"*"===w,R="?"===w||"*"===w,k=n[2]||s,$=g||v;r.push({name:m||o++,prefix:d||"",delimiter:k,optional:R,repeat:E,partial:b,asterisk:!!y,pattern:$?u($):y?".*":"[^"+a(k)+"]+?"})}}return i<e.length&&(c+=e.substr(i)),c&&r.push(c),r}function o(e,t){return s(r(e,t))}function i(e){return encodeURI(e).replace(/[\/?#]/g,function(e){return"%"+e.charCodeAt(0).toString(16).toUpperCase()})}function c(e){return encodeURI(e).replace(/[?#]/g,function(e){return"%"+e.charCodeAt(0).toString(16).toUpperCase()})}function s(e){for(var t=new Array(e.length),n=0;n<e.length;n++)"object"==typeof e[n]&&(t[n]=new RegExp("^(?:"+e[n].pattern+")$"));return function(n,r){for(var o="",s=n||{},a=r||{},u=a.pretty?i:encodeURIComponent,f=0;f<e.length;f++){var h=e[f];if("string"!=typeof h){var p,l=s[h.name];if(null==l){if(h.optional){h.partial&&(o+=h.prefix);continue}throw new TypeError('Expected "'+h.name+'" to be defined')}if(v(l)){if(!h.repeat)throw new TypeError('Expected "'+h.name+'" to not repeat, but received `'+JSON.stringify(l)+"`");if(0===l.length){if(h.optional)continue;throw new TypeError('Expected "'+h.name+'" to not be empty')}for(var d=0;d<l.length;d++){if(p=u(l[d]),!t[f].test(p))throw new TypeError('Expected all "'+h.name+'" to match "'+h.pattern+'", but received `'+JSON.stringify(p)+"`");o+=(0===d?h.prefix:h.delimiter)+p}}else{if(p=h.asterisk?c(l):u(l),!t[f].test(p))throw new TypeError('Expected "'+h.name+'" to match "'+h.pattern+'", but received "'+p+'"');o+=h.prefix+p}}else o+=h}return o}}function a(e){return e.replace(/([.+*?=^!:${}()[\]|\/\\])/g,"\\$1")}function u(e){return e.replace(/([=!:$\/()])/g,"\\$1")}function f(e,t){return e.keys=t,e}function h(e){return e.sensitive?"":"i"}function p(e,t){var n=e.source.match(/\((?!\?)/g);if(n)for(var r=0;r<n.length;r++)t.push({name:r,prefix:null,delimiter:null,optional:!1,repeat:!1,partial:!1,asterisk:!1,pattern:null});return f(e,t)}function l(e,t,n){for(var r=[],o=0;o<e.length;o++)r.push(g(e[o],t,n).source);var i=new RegExp("(?:"+r.join("|")+")",h(n));return f(i,t)}function d(e,t,n){return m(r(e,n),t,n)}function m(e,t,n){v(t)||(n=t||n,t=[]),n=n||{};for(var r=n.strict,o=n.end!==!1,i="",c=0;c<e.length;c++){var s=e[c];if("string"==typeof s)i+=a(s);else{var u=a(s.prefix),p="(?:"+s.pattern+")";t.push(s),s.repeat&&(p+="(?:"+u+p+")*"),p=s.optional?s.partial?u+"("+p+")?":"(?:"+u+"("+p+"))?":u+"("+p+")",i+=p}}var l=a(n.delimiter||"/"),d=i.slice(-l.length)===l;return r||(i=(d?i.slice(0,-l.length):i)+"(?:"+l+"(?=$))?"),i+=o?"$":r&&d?"":"(?="+l+"|$)",f(new RegExp("^"+i,h(n)),t)}function g(e,t,n){return v(t)||(n=t||n,t=[]),n=n||{},e instanceof RegExp?p(e,t):v(e)?l(e,t,n):d(e,t,n)}var v=e("isarray");t.exports=g,t.exports.parse=r,t.exports.compile=o,t.exports.tokensToFunction=s,t.exports.tokensToRegExp=m;var x=new RegExp(["(\\\\.)","([\\/.])?(?:(?:\\:(\\w+)(?:\\(((?:\\\\.|[^\\\\()])+)\\))?|\\(((?:\\\\.|[^\\\\()])+)\\))([+*?])?|(\\*))"].join("|"),"g")},{isarray:14}],16:[function(e,t,n){!function(){var e=Cache.prototype.addAll,t=navigator.userAgent.match(/(Firefox|Chrome)\/(\d+\.)/);if(t)var n=t[1],r=parseInt(t[2]);e&&(!t||"Firefox"===n&&r>=46||"Chrome"===n&&r>=50)||(Cache.prototype.addAll=function(e){function t(e){this.name="NetworkError",this.code=19,this.message=e}var n=this;return t.prototype=Object.create(Error.prototype),Promise.resolve().then(function(){if(arguments.length<1)throw new TypeError;return e=e.map(function(e){return e instanceof Request?e:String(e)}),Promise.all(e.map(function(e){"string"==typeof e&&(e=new Request(e));var n=new URL(e.url).protocol;if("http:"!==n&&"https:"!==n)throw new t("Invalid scheme");return fetch(e.clone())}))}).then(function(r){if(r.some(function(e){return!e.ok}))throw new t("Incorrect response status");return Promise.all(r.map(function(t,r){return n.put(e[r],t)}))}).then(function(){})},Cache.prototype.add=function(e){return this.addAll([e])})}()},{}]},{},[13])(13)});


// *** End of auto-included sw-toolbox code. ***



// Runtime cache 配置转换后的 toolbox 代码.

toolbox.router.get("/*", toolbox.cacheFirst, {"origin":"unpkg.com"});
toolbox.router.get("/npm/*", toolbox.cacheFirst, {"origin":"cdn.jsdelivr.net"});





/* eslint-enable */
