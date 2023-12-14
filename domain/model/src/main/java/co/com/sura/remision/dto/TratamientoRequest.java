package co.com.sura.remision.dto;

import co.com.sura.remision.entity.datosremision.Medicamento;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class TratamientoRequest {
    @JsonIgnore
    private String idTratamiento;
    private TipoTratamientoRequest tipoTratamiento;
    private Medicamento medicamento;
    private long cantidadDosis;
    private UnidadDosisRequest unidadDosis;
    private ViaAdministracionRequest viaAdministracion;
    private FrecuenciaRequest frecuencia;
    private long duracion;
    private Boolean noPBS;
    private TipoPrestacionRequest tipoPrestacion;
}
