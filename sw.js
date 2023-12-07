/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","56680ad4a21bd5c06603ab605615fceb"],["/about/index.html","b534c348b81547237bdb0471e65dff81"],["/archives/2023/01/index.html","334aad1fdfaa341fcfab3e7fc4f8b135"],["/archives/2023/02/index.html","3e010b298a0a1bf9a5fd4edb8ccaed92"],["/archives/2023/02/page/2/index.html","27dadb2b92fff0d0b6d53f3766a069ca"],["/archives/2023/03/index.html","dc4ead3332652590f1a48922e6be00bf"],["/archives/2023/05/index.html","58f8efdb40ee7f927bb23b11a80616ff"],["/archives/2023/06/index.html","f995553b3e173774129e7aef6e693b36"],["/archives/2023/09/index.html","c90770a2fa35f3a0ed7939a08754a1dc"],["/archives/2023/11/index.html","2794d01bcf45f2cfda2e4fdca151387c"],["/archives/2023/12/index.html","26eb612bd9b94b5d8c0b4c2215263891"],["/archives/2023/index.html","758240f7effde61a8e766c44a4a958a7"],["/archives/2023/page/2/index.html","beb26c7e08390bc1a543440532c59242"],["/archives/2023/page/3/index.html","c8ea54c0de95c021714179111b1c95ec"],["/archives/2023/page/4/index.html","2a1ce9532e43eac3c9e51b9856853b33"],["/archives/index.html","756da06556091d0ecd0559f497adae81"],["/archives/page/2/index.html","f3b8a3b5fe693cbaf5d67e03a49917c7"],["/archives/page/3/index.html","94a1fd937931d0293343605a86a03230"],["/archives/page/4/index.html","febfb32f8ca869370286272a2ed4f111"],["/baidu_verify_codeva-qQP2iZOMLX.html","09eda287f125b8e7393d91560615b6db"],["/categories/Java/index.html","829c18d9ea33ce1c14850f2717b3aa5f"],["/categories/Java/后端/index.html","723d63c90e78820bb46e4acab46bb69d"],["/categories/Java/基础/index.html","6fa10a6171a00a834f5d3640e9ce4137"],["/categories/Java/基础/集合/index.html","fc731be941a89bb2812c08d303b3109c"],["/categories/Python/index.html","661fb6216eb6af86797ada1d416c3072"],["/categories/Python/编程环境/index.html","f6e6e0df0c196ad87da59b3d6812b6f3"],["/categories/R语言/index.html","b2ef011efdd2ef2492739b921c6a5a9d"],["/categories/R语言/编程环境/index.html","59627a9289777ffada50f978c2b3713e"],["/categories/index.html","d6528cd274e9cd6a5d0fb103bb48c23c"],["/categories/中间件/index.html","374f62ab60c0cdc87bfdcac8c8a1fd4b"],["/categories/前端/Vue/index.html","6a0a52687c027a110983b0376c588466"],["/categories/前端/index.html","f5edfc4431e120334ddbc0498944c5d1"],["/categories/大数据开发/ElasticSearch/index.html","e79b455a3bcbd35a07679c291cb4b3b7"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","168516b58a7fcd53c2aa5e19e05cf9c7"],["/categories/大数据开发/HBase/index.html","d87f87931d80cd86f82aef8b22619798"],["/categories/大数据开发/HBase/学习笔记/index.html","9932e679134c5607e493d28b76c983ed"],["/categories/大数据开发/HBase/环境搭建/index.html","93a423a180c866dc009fd3c1bd4e2c8a"],["/categories/大数据开发/Hadoop/index.html","e483f5dc28d0e97ab48d432360310788"],["/categories/大数据开发/Hadoop/技术/index.html","87ea8dac8b1a67ebb35a23e4e0707abc"],["/categories/大数据开发/Hadoop/环境搭建/index.html","42b034a2fdd4df55786cd0ed0d52d595"],["/categories/大数据开发/Redis/index.html","e168bbb26cb99cfd825304d71072f8e0"],["/categories/大数据开发/Redis/技术/index.html","f7ca934366ab61d7dee376fb4bee90c3"],["/categories/大数据开发/Redis/环境搭建/index.html","2b43d7b867eafad3cfa5e9fdf4ff2821"],["/categories/大数据开发/Spark/index.html","598eb88bb1e6761503ff072f8374c6a2"],["/categories/大数据开发/Spark/环境搭建/index.html","611c3b563e2b61523277e51ebd007b4d"],["/categories/大数据开发/Zookeeper/index.html","f664b296cd5b7d9d490a72a2f373cae4"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","61500391c2d43a9bd3aa6db75a017005"],["/categories/大数据开发/index.html","4a53d638045ebb43d7c6c7df0244c7de"],["/categories/学校课程/index.html","b3e87a3cc665b4bb8712f3cab2aaf51d"],["/categories/学校课程/计算机操作系统/index.html","4a2a4bf8d1a1f035f19c8024b13a6332"],["/categories/操作系统/Linux/index.html","3bea10caf29582fa656133aafaebfcb2"],["/categories/操作系统/Mac/index.html","67e53da59a60eb4fe1bf55217495d457"],["/categories/操作系统/Windows/index.html","76adce8e53f0631f0561abc1ac9fe178"],["/categories/操作系统/index.html","60dfbf1b688e833c7c8bbd53903d2e72"],["/categories/数学建模/index.html","8ae80bc1033a017490f3e2bc9ddfacc1"],["/categories/数学建模/latex/index.html","93f334b357a2bb3f93abfcb4dbd93570"],["/categories/数学建模/优化类/index.html","e8ad0030bb7d8634231243d492559584"],["/categories/数学建模/优化类/现代优化算法/index.html","1c0d8672f7cb8e5d8a38861a0c093dcb"],["/categories/数学建模/优化类/规划类/index.html","438a9d5827ccbb1716dea3971d6dd0ca"],["/categories/数学建模/绘图/index.html","35ee69a0e59680a39b1439b2ec34c3ba"],["/categories/数据库/MySQL/index.html","f0a62012060632a23edec9440a44a474"],["/categories/数据库/index.html","04bf4705af2d18ecb8faf446095eae9a"],["/categories/数据结构和算法/index.html","471ea7d5067746d4a31116240d4ecd8a"],["/categories/数据结构和算法/page/2/index.html","799026a420cb20a605740b75d6f5276c"],["/categories/数据结构和算法/基本原理/bfs/index.html","ee8393e0a5be84d80248491ab48fa7ee"],["/categories/数据结构和算法/基本原理/dfs/index.html","dd1ef2d9380895ff73fc248d9683447a"],["/categories/数据结构和算法/基本原理/index.html","6e770cc1f6b5e538583718ff1321b5ec"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f8e1a2d602fa7640aecd7a4894094548"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","f753fc8f913a0e60a18098d9fd699fe8"],["/categories/数据结构和算法/基本原理/图论/index.html","5b687002ff48169a3243c27aebae3236"],["/categories/数据结构和算法/基本原理/字符串/index.html","79a745be5825df1e593e24ea8f2a045e"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","13e955e55060d79a4e02e112dba48ced"],["/categories/数据结构和算法/基本原理/数论/index.html","da98c9fb672b48e17f291fa0e09629c6"],["/categories/数据结构和算法/基本原理/树论/index.html","ad58f2e47da077be9c6f234d60a38a02"],["/categories/数据结构和算法/基本原理/链表/index.html","d68937cec04d13bd6c8bb37c802419c0"],["/categories/数据结构和算法/算法题/index.html","f9ccd2d463cb3ac5dc7646c54b6a3e4e"],["/categories/数据结构和算法/算法题/二分查找/index.html","3c5d1409c4b8917f8415e1b0ff11a3d5"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","ef0a92b3d804b402a4792254cb51efae"],["/categories/数据结构和算法/算法题/动态规划/index.html","70a9e487bcf4ea64cc0d2354ad37a420"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","5041323090207368bc9d6fb243f5dca6"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","b0ba240952a6a2d70e1750a691231bb4"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","a6b5a373996e87facc2ea0c895fd4f94"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","9a856517cd978dc8027e7ee06553f2e9"],["/categories/数据结构和算法/算法题/栈和队列/index.html","d57f2c87323b2d2ac535e0d609a2efd4"],["/categories/数据结构和算法/算法题/树论/index.html","b1526e34bf915ff09f23760335d0c685"],["/categories/杂七杂八/index.html","9254f5acb06ceead9b2d54be4f8740fe"],["/categories/杂七杂八/博客搭建/index.html","575a915a281193fc76b0d3aadec7cbf4"],["/categories/编程工具下载/index.html","7c9e024335bb0b2f6b330668db4b1016"],["/categories/编程环境/index.html","de974648dabf5880fcf295fe9c27bd8a"],["/categories/编程环境/大数据/index.html","3ec63ec76f41160a7837c9c4b7978b03"],["/categories/英语学习/index.html","9b523cd4fbde9e235d616a47e3f4ba53"],["/categories/英语学习/英语语法/index.html","1bb38e35949faf89f0cbede86e29d026"],["/comments/index.html","3cec451a7aaab899f112e2112ca1f203"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","d1a3ad32f73ec6d45b6b78528aea6061"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","ffb731e476608f013eb91346b1fe9b94"],["/movies/index.html","e6f8442c6a3efe26f5198e5ce88fa840"],["/music/index.html","ca69780951852929f2fca6178e169ffb"],["/page/2/index.html","d908448413e089d1e7981b0d74249a24"],["/page/3/index.html","0fbc062ff68d82e0cf2166c13b0bbcce"],["/page/4/index.html","ff79cc25ec83fe6f64050101a2caa21d"],["/page/5/index.html","5697c4ab04e90cedf69797e512bcc282"],["/page/6/index.html","3be60d6a89712e9aca5c9fc461fa2e20"],["/posts/1021360842.html","2fe1b49854d454fd2406c9d3dd7d1442"],["/posts/1120620192.html","d5bac19f850bdbf6fabe9f2dd619fc4f"],["/posts/1141628095.html","27f636883f1266ee708f93778b77e205"],["/posts/1168613674.html","6b96d4ed681cf9d73a181537b81ce85d"],["/posts/1219920510.html","871888a27a24d17b10eac50a995e80d8"],["/posts/1222166338.html","75a646785db5da2aa4482ac615f4e262"],["/posts/1259097482.html","4d1af28c9274158e9f82cc5d68ef4d27"],["/posts/1271036369.html","52e49cfb54267bb059639f7bda437cb3"],["/posts/1312847445.html","1dccc6fff6d03719079986c25623fdf1"],["/posts/135355774.html","965e2774a682cfbd0ba88d318080be7b"],["/posts/1375344716.html","2289c30ac31531710b3a128d4dbf664f"],["/posts/1388991698.html","0badb9e59393366827053fef248703de"],["/posts/1410315814.html","000bf8dca9e05d677619bc0195961379"],["/posts/1452790229.html","29c920b668f1018eae36224cdc627d69"],["/posts/1470079884.html","ed362ab9c441b64348f63b47ab42c507"],["/posts/1470079885.html","0a8f60cabbb660c4f83413c39692878a"],["/posts/1470079886.html","e923942764adf6f07329ae78ffa1d21a"],["/posts/1470079887.html","539f01e75811c4e4e76253538b65ca16"],["/posts/1498536549.html","23e91662a291915ac187ec9e08fb6a09"],["/posts/1547067935.html","d408a76ee3b25b115fc85c4948f1d7a6"],["/posts/1557866301.html","a39bcce1878777a0431bfb82a376e3f5"],["/posts/1571776361.html","d78fb2009f02d453c11c1e00ceb3c05b"],["/posts/1605124548.html","1fbfb6894468343e18fec3e59e872c11"],["/posts/1633036852.html","31497e44f6153e72d659de471db91bd0"],["/posts/1674202625.html","3c2120442bc496345d9f524022f60615"],["/posts/1765123828.html","3e80ad4f1227983cc9dbdb69972c404a"],["/posts/1767336200.html","a617f38880cd4854b6189a1d0767e424"],["/posts/1776114197.html","b6b4694c1bb7abc193d00e6071b4848d"],["/posts/1817748743.html","6bc2028d014cb3a2548b20344dd50558"],["/posts/1925125395.html","3fd0dc4ece889bb86a17ec526b978935"],["/posts/1966191251.html","6ad2166b0e48a80d7671d5927e251661"],["/posts/1987617322.html","711afc0ebc03036d22d64e1999f58afb"],["/posts/1999788039.html","77015ec294686e918fba9d59195e3f09"],["/posts/2075104059.html","8f8a9f9be5bd84bb4f4ad62e8ea2e103"],["/posts/2087796737.html","33e4416220f53f084fb1620fb1a5ad0e"],["/posts/2106547339.html","e4aa2771f1d8e0736cdf74392c225f72"],["/posts/2207806286.html","3f4291d9e1e7206fe0106fedc410b08c"],["/posts/2225903441.html","54c0ee907f3098e06f4c7ef431cd394f"],["/posts/2265610284.html","9f643f688483f85259879b22ff347a0b"],["/posts/2281352001.html","3dd980c134cc380c902437aa1e162123"],["/posts/2364755265.html","6029b7a1b028b808ad983372dbeab752"],["/posts/2414116852.html","3dccf67cbe015146d55f256a4f45eb21"],["/posts/2421785022.html","a0e6d80bd3c55f590c2358ed86799de4"],["/posts/2482902029.html","80e1358a5bb23447bbbf75d7727e5de2"],["/posts/2495386210.html","1cf45971b6ac0adc2a35665a6392733c"],["/posts/2516528882.html","f21ee9b53f3c920654bc07d2cca54207"],["/posts/2526659543.html","9180f63f0bda89ea8cb39d0eb836763f"],["/posts/2529807823.html","a5c70d7a21a10003f946d26b0086c3b7"],["/posts/2596601004.html","1c75a4264da12dcc71a9141066012f05"],["/posts/2742438348.html","a3b0cfa06715c1b13c075cda55148215"],["/posts/2864584994.html","e2caa853ffebcca4191aab3d95603de4"],["/posts/2888309600.html","346ac86a39eaca0531f8fe5931d35b3f"],["/posts/2891591958.html","c937f4ea46e76ee21d36bbee9e3d338c"],["/posts/2909934084.html","5176cce4c68ef0b481bd3d3bdf5b2e4f"],["/posts/2920256992.html","7e1f33c4f1829a43f6a9ccdbc76aa34f"],["/posts/2959474469.html","ab03c6ee8b0ecdb2ae66415bcbec52c1"],["/posts/3005926051.html","66926065d19888f73255683fd7431444"],["/posts/309775400.html","7bcfb4d537f2cfffc1c4a5c2e6bbe848"],["/posts/3156194925.html","86b33b0503b80e034440e2f87d1c9886"],["/posts/3169224211.html","67334c97d5d48cf8cbcd9edf78a1a91e"],["/posts/3213899550.html","105668412581c7edbb6e91fafa4bad3f"],["/posts/3259212833.html","14604cd0e394c7c0ba95a6de10ad3646"],["/posts/3266130344.html","6095da6e05f413f091d29bc8415453cb"],["/posts/3292663995.html","5a7b4508cd62cf3539a48e3ec50049f5"],["/posts/3297135020.html","c91b890d895c1455c397ad3f18787b5a"],["/posts/3306641566.html","a587ffafd2ace2798ca4bd68eb0881dc"],["/posts/3312011324.html","a4a65ff5f6866c8eb0e38a2c8e69d6be"],["/posts/336911618.html","d9a89ff6c0111c53fd1ac280f3ad2d73"],["/posts/3402121571.html","3c70bf3472869d73285fc3048e0352e6"],["/posts/3405577485.html","008500a6bce73f3ab10517cb63e1c9a3"],["/posts/3498516849.html","aa32b29a960d0034b13906de42115942"],["/posts/3513711414.html","de08e9c2ed54bc813e5dce3f7eebb7ea"],["/posts/3546711884.html","5fd585006d8840b657a0d724cac5babc"],["/posts/3731385230.html","d29f247853eded7bf484e47fd8695ca9"],["/posts/3772089482.html","280469248fe627583bbae8ba2d345aa5"],["/posts/386609427.html","a26dedefdd0975406f0aa43fe54a939b"],["/posts/4044235327.html","a68a34e0bd95f2781e3e74475337ec82"],["/posts/4115971639.html","092750d513350adc767ae65c39ed069d"],["/posts/4130790367.html","aee7a63aad0bbfef93839b06f5ddd0ad"],["/posts/4131986683.html","f7a400c70084fde91c6f49beb3d30fcf"],["/posts/4177218757.html","0b7277c0259e131486b62b5e5e18edc5"],["/posts/4192183953.html","fd9b535e08ebf43801b7502eb4763b7f"],["/posts/4261103898.html","529282fe44b63dc45c2553459fadd8ce"],["/posts/469711973.html","71d893f9fea9b4d335d36ccc45cbe7c8"],["/posts/482495853.html","d6c7c82968ee15e09153432fa0bcac41"],["/posts/488247922.html","6c8f5428ef4cd651812f9220acab4f5d"],["/posts/517302816.html","e023fe2dc1dc7b6349baccea471a6cc6"],["/posts/570165348.html","20ac9bdd554efa0dfd6a66bf631b339e"],["/posts/595890772.html","539465b83c60a6603298c7df4ac3fd5b"],["/posts/67485572.html","d9092549f6ced1bac1de7c5e8879fb93"],["/posts/694347442.html","50cf95acbbf2ec45e4a685d7e410cc78"],["/posts/707384687.html","b788e774798a98c5d17febeb5beef82f"],["/posts/71180092.html","55a619a0348841d149cace8f83cbfd54"],["/posts/716459272.html","fa41ffb92d90a4012cd855b3d3d090a3"],["/posts/765481613.html","451179567e57c0c089990c2545f879e2"],["/posts/778231993.html","e718738db96d9bea5805a31e86e00b3f"],["/posts/795397410.html","8f606f45ef5d85d7a96fea5f931191bf"],["/posts/820223701.html","cbcb9b9a1489b85bbe762e88931f9830"],["/posts/830372185.html","7667499ebbac7029f46ac2f05e797a47"],["/posts/88294277.html","2ad5de887000c7fa428caea5d5149a8e"],["/posts/939963535.html","0c7117b88bf4a2dcabe309273216c9b3"],["/posts/983786067.html","280cc060ae81e61e86b06d316f5c2abe"],["/sw-register.js","9cbf72379ad8c0e5d580cfc85dc1ef41"],["/tags/C/index.html","d854d640ae38d7e880d0fbb9e5661f33"],["/tags/C/page/2/index.html","418cb367cca6c197c36404a380536484"],["/tags/C/page/3/index.html","d7464b5b68e6d8d40e366a36cfb3393c"],["/tags/ETL/index.html","86f0b08c9ab63787db37b9e9ddf11cee"],["/tags/ElasticSearch/index.html","693c584bb1e6af32813308c3a6b2f9c9"],["/tags/GUI/index.html","f315e4969f251d863f645a8ec9464ede"],["/tags/HBase/index.html","3730f7fc332ade63026bcfcca3c57e40"],["/tags/Hadoop/index.html","c788d406ba612d3fc33107544d44c6cb"],["/tags/Hadoop/page/2/index.html","1a5e0a01a76112ad1abac1a62600e757"],["/tags/Java/index.html","adcd9b1f014610e914b0982ebcb45d9e"],["/tags/Java后端/index.html","93399e5eb49913bd69fe57947d5f3f48"],["/tags/Java后端/page/2/index.html","6776e886285f5918ef9694715a71c31f"],["/tags/Java基础/index.html","1db60a0754492eb2eba3bbeee5a85f10"],["/tags/Java基础/page/2/index.html","30c8d2e55c2919ee49f3378ce21bc513"],["/tags/Kettle/index.html","bc3348037711f456e2da8df09c3e0ff6"],["/tags/Kibana/index.html","76f2572462f8553031c97946d7577f08"],["/tags/Linux/index.html","358fa91e882cfe97bb4c7164c7632e6e"],["/tags/Linux/page/2/index.html","6ddcfbd1709d842c59fe297ddb1b9713"],["/tags/Linux/page/3/index.html","e6bd2d143895de6447d25640a08aa627"],["/tags/Mac/index.html","3ac1dbc5795fa102992268252a6efe59"],["/tags/Mac/page/2/index.html","cc29ae0d5f0285dedc4f6443c2caad6e"],["/tags/Maven/index.html","4a5e929fd1b0aee7440430f0dd556eca"],["/tags/MySQL/index.html","ff66309094102e0107271be58114a707"],["/tags/Python/index.html","571e5791e94bd7cd50ab7851bd9f3534"],["/tags/Redis/index.html","c75e799366a96d8292f11bf78335cbb0"],["/tags/R语言/index.html","33aad3f8f661673b847396569c333dc2"],["/tags/Spark/index.html","63370c273d32679d649c2b066cd161d4"],["/tags/Ubuntu/index.html","60d31c358b5f663c3a782d547fd93300"],["/tags/Vue/index.html","d906f70c2e28144fa2c886beb809921d"],["/tags/Windows/index.html","d970ca4e470232ba95b5303029c056a8"],["/tags/ZooKeeper/index.html","03dd1e8c89aaa6cb7b36f66f0ba5e0c5"],["/tags/bfs/index.html","80a15e04d123aeda3a565e4be771a810"],["/tags/dfs/index.html","9f23b032ae5e7389a0d7ff778f7399e4"],["/tags/folium/index.html","d82c9a5fae5360caab5990c38293bdf0"],["/tags/git/index.html","b09e431c6aae0f813de16efc2206f831"],["/tags/index.html","351bb73f9e5ab7980d5a3fa24c8fc0c7"],["/tags/latex/index.html","b582d5ab8d252d88a2e1f88ad06c2a92"],["/tags/中间件/index.html","36bcbb2eecf93bcc4d31b02df9718b35"],["/tags/二分查找/index.html","c9c13646bf45a283f621c060a2dfd3a8"],["/tags/优化类/index.html","ae1d2785b15502cf730a6e24173eb4f0"],["/tags/前端/index.html","b668986964f81a0110e487c82899fe60"],["/tags/前缀和与差分/index.html","f90c6190d3179f56abbbd3e75c11b183"],["/tags/动态规划/index.html","07d9b248dbe2e6f13486a565abccd2c9"],["/tags/动态规划/page/2/index.html","71a64d498c5f19860d02daa50713e502"],["/tags/博客搭建/index.html","33cfc322903bbd3e0f9a4bdafac34ee2"],["/tags/图论/index.html","023a209c10658b15029920bdc441a4ac"],["/tags/大数据/index.html","5a2b876a805d351e01d51ba626fc7fcb"],["/tags/大数据/page/2/index.html","4d473294a586fdbc28b968f2e4c5d547"],["/tags/操作系统/index.html","741356582f83a13c6a7d9db32aba5c7b"],["/tags/数学建模/index.html","97875dad9bad56a460c19167c1e5f273"],["/tags/数据库/index.html","32bfa0733e2ce81df7b3b242c4ce1453"],["/tags/数据结构和算法/index.html","c3617fc97ae391afa4d8e06609cc1f84"],["/tags/数据结构和算法/page/2/index.html","458daa819fedf7c1de5534dd95bbf1ea"],["/tags/数据结构和算法/page/3/index.html","c607fe3966a30ee7bff8e44ffdcc15fa"],["/tags/数组和字符串/index.html","d71c26f0999087f878124bacfdbe2d6d"],["/tags/枚举类/index.html","c0f1ec26c9a0948013257f6e859e10c5"],["/tags/栈和队列/index.html","8d265b910d95abb9786f77a03faa413f"],["/tags/树论/index.html","226e7ff212107cedd7cbfa2e6a1b1d3b"],["/tags/测试/index.html","0634a3134feac0a5204d3d14f33aab9c"],["/tags/环境/index.html","46f039bec91a5ccd40fbcc566ddaf708"],["/tags/环境变量/index.html","e715ff282a220ec8a55116642a627c59"],["/tags/绘图/index.html","fb70c8d081f61631cbdd14c9249d5c7d"],["/tags/编程工具/index.html","3ad336f489cb72ff0d5ae42d1a0b41b4"],["/tags/编程环境/index.html","a9eb5392bdb44b69d653c4e306d13daa"],["/tags/网络编程/index.html","30c58282161d04c674f9c5df74811562"],["/tags/英语语法/index.html","aec691ea6a3c182fcf7a8ac6bb028ae2"],["/tags/计算机操作系统/index.html","e2febb226e7ba0ad412097f5768ec3fa"],["/tags/论文/index.html","a980a3f0f9be67430e9c72e0de4193c8"],["/tags/资源下载/index.html","d24964f3c3cb9e5a8b92b683bc31eba9"],["/tags/链表/index.html","5d109ebf11d417c5c57b68d216d50215"],["/tags/集合/index.html","1ab7f70e9dec6c4b27bece19a4ab297d"],["/tags/集群/index.html","2e1207b310c826f0dd0f0d1630f5cb98"]];
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
