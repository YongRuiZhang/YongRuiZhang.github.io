/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","88357b0891ed16dd0fd651e8777238a4"],["/about/index.html","22151f6dce78af0d2924d66419a013a3"],["/archives/2023/01/index.html","825a487200415bf3e2e7ba664b82e597"],["/archives/2023/02/index.html","a7431971e1cb75f5600dd26bf7d8d065"],["/archives/2023/02/page/2/index.html","843d691c1363a6abd2c2450e2d08e082"],["/archives/2023/03/index.html","8e12fd7e571555cc4a0ba7c872e7fd34"],["/archives/2023/05/index.html","322dfb139fd62ab6833151d47664b91d"],["/archives/2023/index.html","6c0d202687658c12fe8802bef409b86c"],["/archives/2023/page/2/index.html","5cbbf44838a358165a8d69323bdf776e"],["/archives/2023/page/3/index.html","555c4e966a9ca9fdfa8fa905aea45e93"],["/archives/2023/page/4/index.html","b7deac071efa17a155c99859d30a224b"],["/archives/index.html","08055299277340149bc04c22d0c830cb"],["/archives/page/2/index.html","0279e88378229a2313cb169e1886774d"],["/archives/page/3/index.html","08998c4402f68e426f7751e6097a1296"],["/archives/page/4/index.html","ee6bfd18c40b9381f83ad4ea85f25e3d"],["/categories/Java/index.html","0ebc17c671b2d951c0f37d06d0384bbc"],["/categories/Java/后端/index.html","49ae00318f4b883c3d2387d612a4088e"],["/categories/Java/基础/index.html","66a9c419442c6822d77b130cc0292aba"],["/categories/Java/基础/集合/index.html","8f9c1d560b25237e95667e67134d09a1"],["/categories/Python/index.html","90b1c10f393bcbb0f9468c90ca1b7eb7"],["/categories/Python/编程环境/index.html","58f5980f1a3e21e8d28057891a399cbf"],["/categories/R语言/index.html","ac4237c57ebb15e5d85dfaabf18b73cb"],["/categories/R语言/编程环境/index.html","62ede7d7d400135fbd2a46ee1bf7803f"],["/categories/index.html","a16194a5fca292e5f35d7925e15662a6"],["/categories/中间件/index.html","05bf730e99aa714e7ad40e87bdf1f8dc"],["/categories/大数据开发/ElasticSearch/index.html","768b08b93dbe619d0312db5db09e7a86"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","cb555894a78b0351f499aa5771e4e84c"],["/categories/大数据开发/HBase/index.html","cfe76fb97f88c4b1ef8858a5cc8825e1"],["/categories/大数据开发/HBase/学习笔记/index.html","221d1ed33ddc1bb060171dcef6f352f6"],["/categories/大数据开发/HBase/环境搭建/index.html","a74173f1b8c7341e1645462e0ff972f3"],["/categories/大数据开发/Hadoop/index.html","2a19dfd70809c331319a08cc3e5d317d"],["/categories/大数据开发/Hadoop/技术/index.html","a8a7ec04c21e092bd14686b478d1b56d"],["/categories/大数据开发/Hadoop/环境搭建/index.html","5d05f304267684e836c25734edc6d4d1"],["/categories/大数据开发/Redis/index.html","f5742e6345faf741033be47e9d61965f"],["/categories/大数据开发/Redis/环境搭建/index.html","5ae0471f6547880826d5324c109d20c2"],["/categories/大数据开发/Zookeeper/index.html","e749f6b2f99a09fe683e5632ec2b0596"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","243158c37fa954264ef297d50b7eb95f"],["/categories/大数据开发/index.html","6d2c081517a586c8fae85a2cfcef5643"],["/categories/操作系统/Linux/index.html","d1ef34bc1a2bfd59367de8587d8b16db"],["/categories/操作系统/Mac/index.html","a3261d05720025188b4d0966bd8e3bcb"],["/categories/操作系统/Windows/index.html","5b21f3c4be8d95bec37fd8b0d21e097b"],["/categories/操作系统/index.html","231c7ed942e894b43575b5948534b717"],["/categories/数学建模/index.html","a3eaaf903821e647ade230c7d7b574f4"],["/categories/数学建模/latex/index.html","e7f8ee314a1a93350227559af413ee08"],["/categories/数学建模/优化类/index.html","cf19c711fbf098b920893ea281d5ae16"],["/categories/数学建模/优化类/现代优化算法/index.html","781fa300124a69286f4392d8ac57c707"],["/categories/数学建模/优化类/规划类/index.html","725504b6688e8bf2eeb7c473f6f38e10"],["/categories/数学建模/绘图/index.html","21c0eeaf9892bed155b3fd9ed2d66bb4"],["/categories/数据库/MySQL/index.html","b9804896b103cc785f3693da6ae9c7b5"],["/categories/数据库/index.html","9f2ab1e623f44a63d3f9bf54738aedde"],["/categories/数据结构和算法/index.html","5886ae74e5b30298d5e60dc0f5997525"],["/categories/数据结构和算法/page/2/index.html","4a6f798a8873b91990726be76c17b52f"],["/categories/数据结构和算法/基本原理/bfs/index.html","17a84f1391be2ec085bb15641ecadcc1"],["/categories/数据结构和算法/基本原理/dfs/index.html","7a1b7da66f79455bcff6f79a3db95e68"],["/categories/数据结构和算法/基本原理/index.html","c8b3e84a8b8e59c0bc02d66b23c5edb4"],["/categories/数据结构和算法/基本原理/动态规划/index.html","3f02c0b2cb721f08a88c0582888596a6"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","e4f35a5eb7810a0b2a51ed93d57a05fb"],["/categories/数据结构和算法/基本原理/图论/index.html","9f2e5b6d062f03e444315a67ee481288"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","4156d0087f1d401d71c3d0b2936db34a"],["/categories/数据结构和算法/基本原理/数论/index.html","5a261681c4748e1db867c926be66072b"],["/categories/数据结构和算法/基本原理/树论/index.html","912c0fe0df1868785617d5e0db313dd6"],["/categories/数据结构和算法/基本原理/链表/index.html","4e68463fc797715855ca3c716f9a9159"],["/categories/数据结构和算法/算法题/index.html","62696ccf8e5f6f66c639d8cc56046d2f"],["/categories/数据结构和算法/算法题/二分查找/index.html","5f858e0828edcbb8e6f0c6d9f98aabc7"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","6057403fda35887fc608327bd8c4f92a"],["/categories/数据结构和算法/算法题/动态规划/index.html","4ebda2fd10b51a76c7be7074e93de33c"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","5b0bba093430d94c25f6789a2ffcb0e1"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","50232dff5c44c946104fec2da86b2b91"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","47369eb0c31e7638aecf9e665a4f3b4d"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","a3eceee277591a104d3ed2fddd1d4427"],["/categories/数据结构和算法/算法题/栈和队列/index.html","04cbe3c03db769eb1f9e1d7ed2270f6f"],["/categories/数据结构和算法/算法题/树论/index.html","e129cb8111045b9df0449fd66616925f"],["/categories/杂七杂八/index.html","522807a813074bfd132d211f6b378f46"],["/categories/杂七杂八/博客搭建/index.html","50ff64e8a4efc037df63096fe2d75d5b"],["/categories/编程环境/index.html","9bc57b1425a50e3a641a12e15fb05462"],["/categories/英语学习/index.html","50fd32adcd167ee06daee4eadcf700c4"],["/categories/英语学习/英语语法/index.html","94aff069134536544397b446cc0a0d2d"],["/comments/index.html","f2e10ede8f9c3b3500e07f67b08b01f0"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","c4094144becca98b8fe75f788eda193e"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","d12e2e67fe30ac37c74dea68cc4b7130"],["/movies/index.html","d926127fe3302be7a430d4c4f216db56"],["/music/index.html","ac72a6e14ec93627a9499713d2fd8084"],["/page/2/index.html","44f0d20399e0c9077911a1fae69a4246"],["/page/3/index.html","3e8c115b2e704f6ba0c271a586ab9e54"],["/page/4/index.html","04e3c3617c13d05c705ded115cceb927"],["/page/5/index.html","bd959eceb5c5b17b0d8888088208e452"],["/posts/1021360842.html","abd3d1d1f079977a1d3c98e65c12ecd6"],["/posts/1120620192.html","cdfc4e006ed8fea7f66ebc5dc0bcd752"],["/posts/1141628095.html","856400e75f1dcba543af098731bebac5"],["/posts/1168613674.html","fe283078c475b1dae7e0b1d19e7c9613"],["/posts/1219920510.html","4f2e7f98f8571cf977db067933ec27c7"],["/posts/1222166338.html","fedbd9ee046d6c3bff26896445abe0db"],["/posts/1259097482.html","ea8ac30f0bf965ca722bd5465c0011e0"],["/posts/1271036369.html","9bb7a0a18b1280134bd179392e3042e6"],["/posts/1312847445.html","146c88f5507026efc8c67e371b700bf0"],["/posts/135355774.html","3c6a5aa8709eac9ca9f53937c29df5b8"],["/posts/1375344716.html","bc0932591c86a34ffa767a5d2c914292"],["/posts/1388991698.html","351eed1926488b0cf867c291c959c41d"],["/posts/1410315814.html","3c39632daae27e8e7a3a21f7258ceb4b"],["/posts/1452790229.html","a06848638c5715b19b00d77980668cdc"],["/posts/1470079884.html","40fdd5e3f9c1f9d1a44a090ce1368f22"],["/posts/1470079885.html","5765d69009fa8b088c35597dbc989943"],["/posts/1470079886.html","26f6377e8d1967d6b654f218661e3e05"],["/posts/1470079887.html","5c172b7be39e5dc29bdb587bf7a6966d"],["/posts/1498536549.html","41de460a5cb2da3bfd393cdee1ad14bb"],["/posts/1547067935.html","73a8eca2afb02dba00563546af3e4f4b"],["/posts/1557866301.html","8d2dceffa35a844a5fd65c36a925204c"],["/posts/1571776361.html","773f23b8b5d4c8ae95b60b64a8d6a8fb"],["/posts/1605124548.html","4f8d52c80e027943a39e50e97fe40323"],["/posts/1633036852.html","a11a3a1123994262ccc54bd8a42f1955"],["/posts/1765123828.html","4f26ad75e299ed2b014bc95069054a1b"],["/posts/1776114197.html","b64565dee25932c0b8fa114032b0ef51"],["/posts/1817748743.html","3157002008db136bd81792ef82bfa8d8"],["/posts/1925125395.html","8bfb1628fcb605913463bd918fdde6e7"],["/posts/1966191251.html","956e8b410db2ba9176bae64d91d05728"],["/posts/1987617322.html","8659a568a2a4337e7e61500f35971b82"],["/posts/1999788039.html","aa71bcfd8dc4acd719eb8c7a404a7f6f"],["/posts/2075104059.html","a12b7c77f3d58496925845f668bb3386"],["/posts/2087796737.html","f4d7e8b75b085a9a3f0627699d478081"],["/posts/2106547339.html","854e20b6f57caad91baada761a276413"],["/posts/2207806286.html","47b6e71f6176251d31bc8b257750c7d6"],["/posts/2225903441.html","bb6d7bd6a5c27c58d823a9c2eefca49b"],["/posts/2265610284.html","c0a81d86feaa6445863f06f2ec0d3407"],["/posts/2281352001.html","691d0e6641677df8766536a8bc927a7a"],["/posts/2364755265.html","96886a00c2ca03226ba12cf0a1efcf25"],["/posts/2414116852.html","f2427de8ed4282f549955856a8de4529"],["/posts/2482902029.html","9960a8ba99777a17b0e88d1892c42869"],["/posts/2495386210.html","6d460cc58b61bd1c0ccf8e3299dc8339"],["/posts/2516528882.html","e6dc79364a5145c435f360a5f8eaf241"],["/posts/2526659543.html","b7cb13d06050e8151cae314d3af06bb4"],["/posts/2529807823.html","43fc2b22fdbd4935777dd69360e35ece"],["/posts/2742438348.html","765b6421eba46857fab0faec9f9ffdb5"],["/posts/2888309600.html","52056e56e539018e7c084f595962b1d3"],["/posts/2891591958.html","7df3e4ba6c21e029ae03fe07d31ff8ea"],["/posts/2909934084.html","2a4717cf8d4291f65c46f07455b5e516"],["/posts/2920256992.html","930ac4d09f5579dd7bd93ade55453b71"],["/posts/3005926051.html","3e92b435495850a2c7fc0c596ff8f7fa"],["/posts/309775400.html","ed1c9a803643ca560beb0c3357a640bd"],["/posts/3169224211.html","07de1e40eb9d5be7a1967d3459c6b5fa"],["/posts/3259212833.html","da89cc51909e795775a30aab8ee9bdab"],["/posts/3266130344.html","25df875940580b1aa63afce500027cfb"],["/posts/3306641566.html","e96ad8a03d3a9e8189e061186c2dcaea"],["/posts/3312011324.html","83fe8ac4d2664cf18fda9a51aef3cd4e"],["/posts/336911618.html","636152df1aef8b267b4bf7eecc9ba5c0"],["/posts/3402121571.html","fdd4a834efe913346c23ac8d97b90350"],["/posts/3405577485.html","57a7c749849129cc523213de0912e257"],["/posts/3498516849.html","1c4c1568a1237573e26ec575fde1165b"],["/posts/3513711414.html","3b474a6e524081ca559b84f6f904a334"],["/posts/3546711884.html","95be6f722a6b3fd38525bb49afec6607"],["/posts/3731385230.html","796cbcc6120871c6fab48e3d09f732f3"],["/posts/3772089482.html","9a32333a57321fe7f26ee445a1dec7ba"],["/posts/386609427.html","6cecdc60d16b64cabccbbe97b3d017d0"],["/posts/4044235327.html","774f3c3b653ac47517d6a9dfa55a0535"],["/posts/4115971639.html","92497d109c46648e4f1dff26b0db3211"],["/posts/4130790367.html","9eef738aab36ec5480970ecee66af01a"],["/posts/4131986683.html","abed1bdba8c507b6a5cb97ae41d69bad"],["/posts/4177218757.html","29b2fb44f822034d5a5eed24c6da15ac"],["/posts/4192183953.html","8df2786eae7b476d4b974e00114fdefa"],["/posts/4261103898.html","fbe8400c2a476bc4df5c1f8cfe7e4f34"],["/posts/469711973.html","bb28874a0610222fc98713f59a525d3f"],["/posts/482495853.html","cbfecb8bdadcde6e927e941886f530f9"],["/posts/488247922.html","85eec56d5e06c25b89bb91b0f49e9b03"],["/posts/570165348.html","3eb0137030ba2ce4af1bcb90516af2b9"],["/posts/595890772.html","f93046d4e09bbeb45b445fe7ef04094a"],["/posts/694347442.html","030cc3933fd6e87d408ceabf8a0f451b"],["/posts/707384687.html","80d3182424767df198195b2490f581ae"],["/posts/71180092.html","6a10c0dcbed7c24251aa6dd067b6476a"],["/posts/716459272.html","0ab9cd3d538952a573c49622895057b6"],["/posts/778231993.html","aee69760d4ca26a66d1541ec7b70f4a9"],["/posts/795397410.html","ba07bc9f385a095d7dc2e7451f13362a"],["/posts/820223701.html","c378269fdfa37f17761a742a0b3141db"],["/posts/830372185.html","e2d170f51711c7b013c2b207821a59bf"],["/posts/88294277.html","a7bf0f2790cbf8cea20019e879054f8f"],["/posts/939963535.html","2c3a95d68081b0aa60c805cacf89fdde"],["/posts/983786067.html","0a38f424abe9a60c121f9c1a45bb42ea"],["/sw-register.js","718cffe4fc6a5db77f1f9883ec76c061"],["/tags/C/index.html","584684d35aef434d9919d077bcb2a699"],["/tags/C/page/2/index.html","5afb7161de6d8160f8499f88786caadb"],["/tags/C/page/3/index.html","619c6ce2baf04cbaf697cf4b072ce400"],["/tags/ElasticSearch/index.html","05a05a0b73044e54bc1c854970015ba8"],["/tags/GUI/index.html","ecd06fb13e8885e340a2fc5a6c5dfef5"],["/tags/HBase/index.html","ad1d7b6acae49a6ec8d05b676d1cc15f"],["/tags/Hadoop/index.html","92559d91e9608137d3ccaf401acdc42b"],["/tags/Hadoop/page/2/index.html","e1ea9ebbf32e503e31b49aaa4c07edbf"],["/tags/Java/index.html","2a1808a038c2741bd521407213f6dda8"],["/tags/Java后端/index.html","b9c345cae4bbea9279e5a9204cf1a23e"],["/tags/Java基础/index.html","7c082c108ffb6e66e791c34f21a3eaa9"],["/tags/Java基础/page/2/index.html","e45966dc762650a1c0b12534a2613460"],["/tags/Kibana/index.html","9a4728fc1d4904857c301467d36aa9de"],["/tags/Linux/index.html","9f647283fe420b9a8db7c1492d0dc759"],["/tags/Linux/page/2/index.html","a49aa1fb1460512a7a43052fb5c1ec6d"],["/tags/Mac/index.html","411751cc42f6960fe07771200b3cbe11"],["/tags/Mac/page/2/index.html","6c7108406ccff33706f5e7749803a304"],["/tags/Maven/index.html","6c2d0a403b41967f907ae52b2525653f"],["/tags/MySQL/index.html","822bdc6a57d497f64e4de9bd7b245cae"],["/tags/Python/index.html","262f86b787eb838f54d639d2e1225521"],["/tags/Redis/index.html","1fb05157ba67fbf89013b6e68dc6eb58"],["/tags/R语言/index.html","3dc20f56ce8aff3bf1fb2740fb4170a2"],["/tags/Ubuntu/index.html","157ba31a91e265a303c816053bedfb62"],["/tags/Windows/index.html","9900bc3f135396f6712e6bf333bb483f"],["/tags/ZooKeeper/index.html","084b8807ac08c278fcba0091d3ef0096"],["/tags/bfs/index.html","2c83eaad7f5ac68ed66424d45516599b"],["/tags/dfs/index.html","c7f5ec749ec788edcca93fcb750bd00b"],["/tags/folium/index.html","b89909f838403ce4d517f3343b61550d"],["/tags/git/index.html","b8a3ffe5fd8f1767f33422ce740ac051"],["/tags/index.html","4007c9094ff2734e57141356e3711fde"],["/tags/latex/index.html","3047b747ca1e6ca154f1c07df999ae13"],["/tags/中间件/index.html","651d01050da49225efd5aa7e87c78700"],["/tags/二分查找/index.html","b0bfeff6437804a39c083ee10ab8006b"],["/tags/优化类/index.html","22d7caf77fb5859d6daf41433262a651"],["/tags/前缀和与差分/index.html","548e3bf3a8d3132cafc1d9f2dda17a2a"],["/tags/动态规划/index.html","a660688ba41e80a384241147a96377d5"],["/tags/动态规划/page/2/index.html","f3d6b517e1e8df7f39c3d21c99c67d91"],["/tags/博客搭建/index.html","1301af90d86e0868c83f365442e6a137"],["/tags/图论/index.html","b9a4cda32cdae32f2b872771bbf0badd"],["/tags/大数据/index.html","17667e0fed79ca0377b3cc43da902e37"],["/tags/大数据/page/2/index.html","1de9bb5ef72e019fd4dbb12892a527c1"],["/tags/操作系统/index.html","d6169f7debe5f20873ffac2e26fe400a"],["/tags/数学建模/index.html","653bc36aaa238c2415377d63021135cf"],["/tags/数据库/index.html","4a2cd250138036dda47e49ba72480c00"],["/tags/数据结构和算法/index.html","0e15c4c73ec13570c7b7172624aacf39"],["/tags/数据结构和算法/page/2/index.html","ceb69041f77244265a76c414af6eec7e"],["/tags/数据结构和算法/page/3/index.html","83cd4ad2aad213b530ebf850a26ac7cf"],["/tags/数组和字符串/index.html","8a72f6681d483a2307c4f25300e32edf"],["/tags/枚举类/index.html","b2fb0821bc4d34072729d91fd0d3540f"],["/tags/栈和队列/index.html","84a7075290c725a4588334b001588073"],["/tags/树论/index.html","d17f8f8dee95b3608c2551bd21054a4f"],["/tags/测试/index.html","534c24868b3c35dc1849d6fe5519333e"],["/tags/环境/index.html","a3721e286a3a9790c946a7188d245f4c"],["/tags/环境变量/index.html","b6514e9059a1604987f33218c375c571"],["/tags/绘图/index.html","35059cb16f903e3d74b13d3eae82dc8c"],["/tags/编程环境/index.html","500c817dc88edb3ac2a1b97b3bd5265d"],["/tags/网络编程/index.html","3e442fe5e93b1bac275b5a40bfe54dd6"],["/tags/英语语法/index.html","fa9419ea631cd0f2539e20d33cc3fe79"],["/tags/论文/index.html","b440aed97c6a3ce96c5b49fa03b3e0d6"],["/tags/资源下载/index.html","e4321787bcd1074b7b5a2735c9d03a82"],["/tags/链表/index.html","0cd4c6738ed6b2b7a20cc0c0c31a39dd"],["/tags/集合/index.html","e007a36d326b0790c4c08273ba1fbc67"],["/tags/集群/index.html","b0d88850fdf5930cce32b9ba650aa71a"]];
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
