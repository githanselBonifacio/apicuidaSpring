package co.com.sura.genericos;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Resultado {
    private String mensaje;
    private Object atributo;

    public static Boolean isNotNull(Resultado resultado){
        return resultado.mensaje!=null && resultado.atributo!=null;
    }
}
