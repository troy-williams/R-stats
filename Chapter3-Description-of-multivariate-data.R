# ��R�́F
# 3.1 ���֊֌W�͈��ʊ֌W�ł͂Ȃ�
# �����𔺂�Ȃ����ʊ֌W�̐��_�͂ǂ����Ă��B�������c��B
# ����v�������ʂɑ΂��ĉe����^���Ă���̂��A���̒��x�𓝌v�f�[�^�Ɋ�Â���
# ���_������@�͓��v�I���ʐ��_�̕���B


# 3.2 �ؒf����
# ����W�c���̂Q�ϐ��ɐ��̑傫�ȑ��ւ������鎞�A���̏W�c��ؒf���ď�����
# �K���ɕ��������A���̏W�c�̑��ւɔ�ׂď������Ȃ錻�ہB ---> �ؒf����or�I������
# ex. �{�N�V���O�͑̏d���ő̏d���d�������������A�̏d�ŕ�����ꂽ�����ł̑��ւ͏������Ȃ�
cor(math, phys)
# ���w�̓_����60�ŋ��E�Ƃ��ĂQ�ɕ����đ��ւ����Ă݂�
cor(math[math<60], phys[math<60])      # 0.23
cor(math[math>=60], phys[math>=60])    # 0.45
# ���w�ƕ����̍��v�_��120�ȏ�̐l�̐��w�ƕ����̑��ւ����Ă݂�
cor(math[math+phys>=120], phys[math+phys>=120])   # 0.18 �قڑ��ւ��Ȃ��B�B�B
# �ؒf���ʂ͎��o���Ă��Ȃ��ƊԈႢ�ɋC�Â��Ȃ����Ƃ��������ߒ��ӁB


# 3.3 �O��l�̉e��
# ���֌W���͓��ɃT���v���������������ɊO��l�̉e�����ɒ[�Ɏ󂯂�
x <- rnorm(20, 30, 10)
y <- rnorm(20, 30, 10)
cor(x, y)               # 0.13

xa <- c(x, 100)         # x��100��ǉ�
ya <- c(y, 100)
cor(xa, ya)             # 0.72

# ���̂悤�ȏꍇ�O��l���������čl���邱�Ƃ������B���̖��͍��[��
# �O��l�ł��邩�𔻒f����ɂ͌��̎��ۂ��ǂ�Ȋm�����z�ł��邩�𕪂����Ă���K�v������
# ����������ɂ͑�ʂ̃T���v�����K�v�ŁA����Ȏ��ɊO��l�炵�����̂������Ă��Ă�
# ���ꂪ�O��l�ł��邩�͊ԈႢ��������Ȃ��B���̊m�����z�����̂悤�Ȃ��̂�������Ȃ����炾�B
# ���̂�����͎��ۂ́u�K���Łv���f�����B

# �@�B�I�ɊO��l��e���p�b�P�[�W ---> robustbase, robust


# 3.4 �O�ϗʈȏ�̃f�[�^�̋L�q
# 3�ϗʈȏ�͎U�z�}�Ŏ��o�I�ɕϗʂ̊֌W���݂邱�Ƃ����� ---> �U�z�}�s��𓱓�
head(airquality)
plot(airquality[,1:4])   # seaborn�ł���pair plot�̂悤�Ȃ���


# 3.5 ���U�����U�s��Ƒ��֍s��
# �e�L�X�g�ɂ͓�����Ə����Ă��邪�v�͑��ϐ��̕��U�Ƒ��ւ̈ꗗ
iris_mat <- iris[,1:4]
# ���U�����U�s�� ---> �قȂ�ϐ��̑g�ݍ��킹�͋����U�A�����ϐ��̑g�ݍ��킹�͕��U
var(iris_mat)

# ���֍s�� ---> �����ϐ��̑g�ݍ��킹�͓��R�P�ɂȂ�
cor(iris_mat)


# 3.6 �͖����
# 3-1 
x <- rnorm(100, 0, 1)
y <- rnorm(100, 0, 2)
a <- c(x[1:50], y[1:50])
b <- c(x[51:100], y[51:100])

cor(x, y)
cor(a, b)

# 3-2
x <- c(1,2,2,3)
barplot(x, names.arg = c("poor", "not bad", "good", "very good"),
        xlab = "feeling",
        ylab = "population",
        width = 0.8,
        legend.text = c("poor", "not bad", "good", "very good"),
        beside = FALSE,
        col = cm.colors(4),
        main = "title only in English",
        sub = "sub title"
        
        )
help("barplot")

# 4.3.3: PDF, probability density function:
 # ---> uniform distribution's PDF : dunif
curve(dunif, from = -1,to= 2)
 # ---> normal distribution's PDF : dnorm
curve(dnorm, -2, 2)

# 4.3.4: CDF, cumulative probability density function
 # ---> uniform distribution's CDF : punif
curve(punif, from = -1, to = 2)
 # ---> normal distribution's CDF : pnorm
curve(pnorm, -1,2)

# 4.3.4: quantile function: 
# ��l���z��0.95���ʓ_�͖��炩��0.95
qunif(0.95,min=0, max=1)    # ---> 0.95
# �W�����K���z�ł�
qnorm(0.95, mean=0, sd=1)   # ---> 95�p�[�Z���^�C���_�B-������0.95%�܂ł̖ʐς̑��a
# �ݐϕ��z�֐��ƕ��ʓ_�֐��݂͌��ɋt�֐��B�����l���Ԃ��Ă���
pnorm(qnorm(0.95))
qnorm(pnorm(0.95))

# 4.3.6: ��l�����̔���
# min < X < max�͈̘̔͂A����l���z�ɏ]�������𔭐������邱�Ƃ��ł���
set.seed(1)
runif(6, min=-1, max=1)    # 6����������

# �����̗����𔭐����������ꍇ ---> �����_�ȉ���؂�̂Ă�
x <- runif(5,1,9)
as.integer(x)





