package co.com.sura.entity.agenda;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Movil {
    private String matricula;
    private String numeroIdentificacionConductor;
    private String idRegional;
    private Boolean activo;
}
