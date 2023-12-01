package co.com.sura.entity.remision.datosremision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class Tratamiento {
    private String tipoTratamiento;
    private Medicamento medicamento;
    private Integer cantidadDosis;
    private String unidadDosis;
    private String viaAdministracion;
    private String frecuencia;
    private Integer duracion;
    private boolean noPBS;
    private String tipoPrestacion;
    private boolean notificado;
}
