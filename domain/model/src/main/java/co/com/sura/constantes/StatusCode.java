package co.com.sura.constantes;

import lombok.Getter;

@Getter
public class StatusCode {

    public static final  Integer STATUS_500 = 500;
    public static final Integer STATUS_200 =200 ;
    public static final Integer STATUS_400 =400;

    private StatusCode() {
        throw new IllegalStateException("Utility class");
    }
}
