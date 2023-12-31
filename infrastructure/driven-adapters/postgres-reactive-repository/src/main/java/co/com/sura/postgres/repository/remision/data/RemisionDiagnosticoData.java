package co.com.sura.postgres.repository.remision.data;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Column;
import org.springframework.data.relational.core.mapping.Table;

import javax.persistence.Entity;

@Builder(toBuilder = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = "remision_diagnostico")
public class RemisionDiagnosticoData {
    @Id
    @Column("id_diagnostico")
    private Integer idDiagnostico;
    @Column("id_remision")
    private String idRemision;
    @Column("nombre_diagnostico")
    private String nombreDiagnostico;
}
