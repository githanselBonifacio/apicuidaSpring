package co.com.sura;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

@ExtendWith(SpringExtension.class)
@SpringBootTest
class MainApplicationTest {
    @Test
     void main() {
        var arg = new String[]{"arg1"};
        MainApplication.main(arg);
    }

}

