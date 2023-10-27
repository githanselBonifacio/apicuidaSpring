package co.com.sura.genericos;

public interface GenericosFactory {

    static Resultado crearResultado(String mensaje, Object atributo){
        return Resultado
                .builder()
                .mensaje(mensaje)
                .atributo(atributo)
                .build();
    }
}
