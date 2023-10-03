package co.com.sura.entity.maestro;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class TipoIdentificacion {
    private Integer id;
    private String idTipo;
    private String nombre;
    private Boolean esMayorEdad;
}
