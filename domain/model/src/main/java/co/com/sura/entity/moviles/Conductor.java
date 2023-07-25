package co.com.sura.entity.moviles;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder(toBuilder = true)
public class Conductor {
    private String numeroIdentificacion;
    private String primerNombre;
    private String segundoNombre;
    private String primerApellido;
    private String segundoApellido;
}
