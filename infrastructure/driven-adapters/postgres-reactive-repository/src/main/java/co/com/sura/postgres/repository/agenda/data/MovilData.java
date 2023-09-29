package co.com.sura.postgres.repository.agenda.data;

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
@Table(name = "moviles")
public class MovilData {
    @Id
    private String matricula;

    @Column("numero_identificacion")
    private String numeroIdentificacionConductor;

    @Column("id_regional")
    private String idRegional;
    private Boolean activo;
}
