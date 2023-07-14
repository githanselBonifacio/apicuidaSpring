package co.com.sura.dto.remision;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder(toBuilder = true)
public class CrearRemisionCitasRequest {
    private RemisionRequest remision;
    private List<CitaRequest> citas;
}
